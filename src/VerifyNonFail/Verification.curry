-------------------------------------------------------------------------
--- A framework-based verification for Curry programs w.r.t. failing
--- computations. A program successfully verified by this tool should
--- never fail at run-time (apart from explicit error) provided that
--- the call types are satisfied when invoking a function.
---
--- @author Michael Hanus
--- @version May 2025
-------------------------------------------------------------------------

module VerifyNonFail.Verification ( nonFailVerifier ) where

import Control.Monad               ( unless, when )
import Control.Monad.IO.Class      ( liftIO )
import Curry.Compiler.Distribution ( curryCompiler )
import Data.Char                   ( toLower )
import Data.IORef
import Data.List
import Data.Maybe                  ( isNothing )
import System.Environment          ( getArgs )
import Text.Pretty                 ( pPrint )

import Debug.Trace ( trace )

-- Imports from dependencies:
import Analysis.Types             ( Analysis, analysisName, startValue )
import Analysis.TermDomain
import Analysis.Values
import Control.Monad.Trans.Class  ( lift )
import Control.Monad.Trans.State  ( StateT, get, put, execStateT )
import Control.Monad.Trans.Reader ( ReaderT, ask, runReaderT )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Functor.Invariant     ( Invariant (..) )
import Data.Time                  ( ClockTime )
import Debug.Profile
import FlatCurry.AddTypes         ( applyTSubst, splitArgTypes )
import FlatCurry.Goodies
import FlatCurry.Names
import FlatCurry.NormalizeLet
import FlatCurry.Print
import FlatCurry.Types
import JSON.Data
import JSON.Pretty                ( ppJSON )
import System.CurryPath           ( runModuleAction )
import System.Directory           ( createDirectoryIfMissing, doesFileExist
                                  , removeDirectory )
import System.FilePath            ( (</>) )
import System.Path                ( fileInPath )
import System.Process             ( exitWith )
import Verification.Env           ( VUFuncEnv, VUProgEnv, currentProg, currentFuncName, currentFunc, currentModule, funcInfoFromEnv, progsFromEnv )
import Verification.Run           ( runUntypedVerification )
import Verification.Options       ( VOptions (..), defaultVOptions )
import Verification.Monad         ( VM, throwVM )
import Verification.State         ( ppVState )
import Verification.Types         ( UVerification, Verification (..), emptyVerification )
import Verification.Update        ( VFuncUpdate (..), VProgUpdate (..), VUFuncUpdate, VUProgUpdate, VUProgUpdate, simpleVFuncUpdate, emptyVProgUpdate, emptyVFuncUpdate )
import XML

-- Imports from package modules:
import FlatCurry.Build
import FlatCurry.Simplify         ( simpExpr )
import Legacy.Run                 ( runLegacy )
import VerifyNonFail.CallTypes
import VerifyNonFail.Conditions
import VerifyNonFail.Files
import VerifyNonFail.Info
import VerifyNonFail.Helpers
import VerifyNonFail.IOTypes
import VerifyNonFail.Options
import VerifyNonFail.ProgInfo
import VerifyNonFail.Statistics
import VerifyNonFail.WithSMT

------------------------------------------------------------------------------

--- The non-failure verifier as a framework verification.
nonFailVerifier :: Options -> IO (Either String (UVerification (VerifyInfo AnyDomain)))
nonFailVerifier opts =
  if did == analysisName resultValueAnalysisTop
    then Right . invmap (TopDomain <$>) (fromTopDomain <$>) <$> nonFailureVerifierWith resultValueAnalysisTop opts
    else if did == analysisName resultValueAnalysis2
      then Right . invmap (D2Domain <$>) (fromD2Domain <$>) <$> nonFailureVerifierWith resultValueAnalysis2 opts
      else if did == analysisName resultValueAnalysis5
        then Right . invmap (D5Domain <$>) (fromD5Domain <$>) <$> nonFailureVerifierWith resultValueAnalysis5 opts
        else return . Left $ "Unknown analysis domain ID: " ++ did
  where did = optDomainID opts

nonFailureVerifierWith :: TermDomain a => Analysis a -> Options -> IO (UVerification (VerifyInfo a))
nonFailureVerifierWith valueanalysis opts = do
  gs <- emptyGlobals valueanalysis
  return emptyVerification
    { vPreprocess = preprocessProg gs
    , vInit       = initFuncInfo opts gs
    , vUpdate     = updateFuncInfo gs opts
    }

preprocessProg :: TermDomain a => VerifyGlobals a -> VUProgEnv (VerifyInfo a) -> VM VUProgUpdate
preprocessProg gs env = do
  prog <- currentProg env

  -- Compute module-level auxiliaries
  let modconsinfos = consInfoOfTypeDecls (progTypes prog)
      orgfdecls    = progFuncs prog
      visfuncs     = map funcName (filter ((== Public) . funcVisibility) orgfdecls)

  liftIO $ do
    modifyIORef (vgsConsInfos gs) $ M.union modconsinfos
    modifyIORef (vgsVisibleFuncs gs) $ S.union (S.fromList visfuncs)

  return emptyVProgUpdate

initFuncInfo :: TermDomain a => Options -> VerifyGlobals a -> VUFuncEnv (VerifyInfo a) -> VM (Maybe (VerifyInfo a))
initFuncInfo opts gs env = do
  let fdecl = currentFunc env
  prog <- currentProg env

  (consinfos, isVisible) <- liftIO $ do
    cis <- readIORef $ vgsConsInfos gs
    vfs <- readIORef $ vgsVisibleFuncs gs
    return (cis, flip S.member vfs)

  -- infer initial abstract call type:
  (_, acalltype) <- inferCallType opts consinfos fdecl
  -- infer initial in/out type:
  (_, iotype) <- inferIOType opts (vgsValueAnalyis gs) (vgsAnalysisStore gs) prog fdecl

  -- TODO

  return . Just $ emptyVerifyInfo
    { viCallType = Just acalltype
    , viIOType   = Just iotype
    }

updateFuncInfo :: TermDomain a => VerifyGlobals a -> Options -> VUFuncEnv (VerifyInfo a) -> VM (VUFuncUpdate (VerifyInfo a))
updateFuncInfo gs opts env = do
  -- TODO: In the legacy implementation the state would be initialized only once for the whole program, not per iteration.
  let state = emptyVerifyState opts
      fdecl = currentFunc env
  state' <- execVerifyM (verifyFunc fdecl) state (VerifyEnv env)
  return emptyVFuncUpdate

--- Global internal state that is held across the whole verification
--- lifecycle in an IORef. Mostly used for non-failure verification-specific
--- caching purposes.
data VerifyGlobals a = VerifyGlobals
  { vgsConsInfos     :: IORef (M.Map QName ConsInfo) -- infos about all constructors
  , vgsVisibleFuncs  :: IORef (S.Set QName)          -- public functions
  , vgsAnalysisStore :: IORef (AnalysisStore a)      -- CASS analysis infos
  , vgsValueAnalyis  :: Analysis a                   -- CASS value analysis
  }

emptyGlobals :: Analysis a -> IO (VerifyGlobals a)
emptyGlobals valueanalysis = VerifyGlobals
                             <$> newIORef M.empty
                             <*> newIORef S.empty
                             <*> newIORef (AnaStore [])
                             <*> pure valueanalysis

--- Infer the initial (abstract) call types of a function in a program and
--- return them together with the number of all/public non-trivial call types.
--- The last argument are the already stored old call types, if they are
--- up to date.
inferCallType :: TermDomain a => Options -> M.Map QName ConsInfo -> FuncDecl -> VM (QName, ACallType a)
inferCallType opts consinfos fdecl = return $ funcCallType2AType consinfos $ callTypeFunc opts consinfos fdecl

--- Infer the in/out types of a function in a program and return them
--- together with the number of all and public non-trivial in/out types.
inferIOType :: TermDomain a => Options -> Analysis a -> IORef (AnalysisStore a) -> Prog -> FuncDecl -> VM (QName, InOutType a)
inferIOType opts valueanalysis astore flatprog fdecl = do
  rvmap <- liftIO $ loadAnalysisWithImports astore valueanalysis opts flatprog
  return $ inOutATypeFunc rvmap fdecl

--- Local internal state.
data VerifyState a = VerifyState
  { vstConsInfos       :: [(QName,ConsInfo)]         -- infos about all constructors
  , vstFreshVar        :: Int                        -- fresh variable index in a rule
  , vstVarExp          :: [(Int,TypeExpr,Expr)]      -- map variable to its type and
                                                     -- subexpression
  , vstVarTypes        :: VarTypesMap a              -- map variable to its abstract types
  , vstCondition       :: Expr -> Expr               -- current branch condition (with hole)
  , vstFailedFuncs     :: [(QName,Int,Expr)]         -- functions with illegal calls
  , vstPartialBranches :: [(QName,Int,Expr,[QName])] -- incomplete branches
  , vstNewFailed       :: [(QName,ACallType a)]      -- new failed function call types
  , vstNewFunConds     :: [(QName,NonFailCond)]      -- new non-failure conditions
  , vstStats           :: (Int,Int,Int)              -- number of: non-trivial calls /
                                                     -- incomplete cases /
                                                     -- SMT-checked non-trivial calls
  , vstToolOpts        :: Options
  , vstError           :: Bool
  }

emptyVerifyState :: Options -> VerifyState a
emptyVerifyState opts = VerifyState
  { vstConsInfos       = []
  , vstFreshVar        = 0
  , vstVarExp          = []
  , vstVarTypes        = []
  , vstCondition       = id
  , vstFailedFuncs     = []
  , vstPartialBranches = []
  , vstNewFailed       = []
  , vstNewFunConds     = []
  , vstStats           = (0, 0, 0)
  , vstToolOpts        = opts
  , vstError           = False
  }

newtype VerifyEnv a = VerifyEnv
  { veFuncEnv :: VUFuncEnv (VerifyInfo a)
  }

--- The local verification monad.
type VerifyM a = StateT (VerifyState a) (ReaderT (VerifyEnv a) VM)

--- Executes the local verification monad.
execVerifyM :: VerifyM a _ -> VerifyState a -> VerifyEnv a -> VM (VerifyState a)
execVerifyM m s e = runReaderT (execStateT m s) e

--- Fetches the verification framework env.
askVFuncEnv :: VerifyM a (VUFuncEnv (VerifyInfo a))
askVFuncEnv = veFuncEnv <$> lift ask

-- Sets the name and arity of the current function in the state.
setToolError :: TermDomain a => VerifyM a ()
setToolError = do
  st <- get
  put $ st { vstError = True }

-- Gets the current function, arity and args.
getCurrentFunc :: TermDomain a => VerifyM a (QName, Int, [Int])
getCurrentFunc = do
  Func qf ar _ _ rule <- currentFunc <$> askVFuncEnv
  case rule of
    Rule vs _  -> return (qf, ar, vs)
    External _ -> return (qf, 0, [])

-- Gets the name of the current function in the state.
getCurrentFuncName :: TermDomain a => VerifyM a QName
getCurrentFuncName = currentFuncName <$> askVFuncEnv

-- Gets information about all constructors.
getConsInfos :: TermDomain a => VerifyM a [(QName,ConsInfo)]
getConsInfos = get >>= return . vstConsInfos

-- Sets the fresh variable index in the state.
setFreshVarIndex :: TermDomain a => Int -> VerifyM a ()
setFreshVarIndex fvi = do
  st <- get
  put $ st { vstFreshVar = fvi }

-- Gets a new fresh variable index.
newFreshVarIndex :: TermDomain a => VerifyM a Int
newFreshVarIndex = do
  v <- fmap vstFreshVar get
  setFreshVarIndex (v + 1)
  return v

-- Runs a function that takes an `IORef ProgInfo` and runs it in the verification environment.
-- This is used to adapt legacy code and should ideally be refactored since it's less efficient
-- than operating on the verification environment directly.
withLegacyProgInfo :: (IORef ProgInfo -> VerifyM a b) -> VerifyM a b
withLegacyProgInfo f = do
  env <- askVFuncEnv
  let pi = ProgInfo $ (\p -> (progName p, prog2ModInfo p)) <$> progsFromEnv env
  pistore <- liftIO $ newIORef pi
  f pistore
  -- TODO: Should we handle changes?

-- Adds a new (more restricted) inferred call type for a function
-- which will be used in the next iteration. If there is already
-- a more restricted call type, they will be joined.
addCallTypeRestriction :: TermDomain a => QName -> ACallType a -> VerifyM a ()
addCallTypeRestriction qf ctype = do
  st <- get
  maybe (put $ st { vstNewFailed = (qf,ctype) : (vstNewFailed st) } )
        (\ct -> do
           let newct = joinACallType ct ctype
           put $ st { vstNewFailed = unionBy (\x y -> fst x == fst y)
                                             [(qf,newct)] (vstNewFailed st) })
        (lookup qf (vstNewFailed st))

-- TODO: Migrate logging to verification framework

-- Adds a new condition (provided as the second argument as a FlatCurry
-- expression) to the call condition for the current function (first argument)
-- so that it will be used in the next iteration.
-- If there is already a new refined call condition, they will be combined.
-- If the condition is `True`, i.e., the call condition cannot be refined,
-- then the current branch condition will be negated and added
-- so that the current branch is not reachable.
-- Furthermore, the call type of the current function is set to
-- always failing.
addConditionRestriction :: TermDomain a => QName -> Expr -> VerifyM a ()
addConditionRestriction qf cond = do
  st <- get
  when (optSMT (vstToolOpts st)) $ do
    (_,_,vs) <- getCurrentFunc
    oldcalltype <- getCallType qf 0
    let totaloldct = isTotalACallType oldcalltype
        -- express oldcalltype as a condition to be added to `cond`:
        oldcalltypecond = aCallType2Bool (vstConsInfos st) vs oldcalltype
    branchcond <- getExpandedCondition
    newbranchcond <- getExpandedConditionWithConj cond
    let newcond = fcAnd oldcalltypecond
                    (if cond == fcTrue
                       then fcNot branchcond
                       else -- current branch condition implies new condition:
                            fcOr (fcNot branchcond) newbranchcond)
    printIfVerb 2 $ "New call condition for function '" ++ snd qf ++ "': " ++
                    showSimpExp newcond ++
                 (if totaloldct then "" else " (due to non-trivial call type)")
    printIfVerb 3 $ "Check satisfiability of new call condition..."
    unsat <- isUnsatisfiable newcond
    when unsat $ printIfVerb 2 $ "...is unsatisfiable"
    vartypes <- fmap (map (\(v,t,_) -> (v,t))) getVarExps
    setNewFunCondition qf (if unsat then nfcFalse
                                    else genNonFailCond vartypes newcond)
  addCallTypeRestriction qf failACallType

--- Sets a new non-fail condition for a function.
--- If the function has already a new non-fail condition, they will be combined.
setNewFunCondition :: TermDomain a => QName -> NonFailCond -> VerifyM a ()
setNewFunCondition qf newcond = do
  st <- get
  maybe (put $ st { vstNewFunConds = (qf,newcond) : (vstNewFunConds st) } )
        (\prevcond -> do
          let newct = combineNonFailConds prevcond newcond
          put $ st { vstNewFunConds = unionBy (\x y -> fst x == fst y)
                                        [(qf,newct)] (vstNewFunConds st) })
        (lookup qf (vstNewFunConds st))

--- Tries to generate a Boolean condition from an abstract call type,
--- if possible.
aCallType2Bool :: TermDomain a => [(QName,ConsInfo)] -> [Int] -> ACallType a -> Expr
aCallType2Bool _       _  Nothing      = fcFalse
aCallType2Bool consinfos vs (Just argts) =
  if all isAnyType argts
    then fcTrue
    else fcAnds (map act2cond (zip vs argts))
 where
  act2cond (v,at) = fcAnds $
    map (\ct -> if all isAnyType (argTypesOfCons ct (arityOfCons (M.fromList consinfos) ct) at)
                  then transTester (M.fromList consinfos) ct (Var v)
                  else fcFalse )
        (consOfType at)


-- Adds a failed function call (represented by the FlatCurry expression)
-- to the current function. If the second argument is `Just vts`, then
-- this call is not failed provided that it can be ensured that the
-- variable types `vts` hold. This information is used to refine the
-- call type of the current function, if possible.
-- Similarly, this call is not failed provided that the third argument
-- (a condition represented as a FlatCurry expression) is not failed so that
-- this condition is also used to refine the call condition of the current
-- function.
addFailedFunc :: TermDomain a => Expr -> Maybe [(Int,a)] -> Expr -> VerifyM a ()
addFailedFunc exp mbvts cond = do
  st <- get
  (qf,ar,args) <- getCurrentFunc
  put $ st { vstFailedFuncs = union [(qf,ar,exp)] (vstFailedFuncs st) }
  maybe (addConditionRestriction qf cond)
        (\vts ->
           if any ((`elem` args) . fst) vts
             then do
               oldct <- getCallType qf ar
               let ncts  = map (\v -> maybe anyType id (lookup v vts)) args
                   newct = maybe Nothing
                                 (\oldcts -> Just (map (uncurry joinType)
                                                       (zip oldcts ncts)))
                                 oldct
               if oldct == newct
                 then noRefinementFor qf
                 else do
                   printIfVerb 2 $ "TRY TO REFINE FUNCTION CALL TYPE OF " ++
                                   snd qf ++ " TO: " ++ prettyFunCallAType newct
               addCallTypeRestriction qf newct
             else noRefinementFor qf
        )
        mbvts
 where
  noRefinementFor qf = do
    printIfVerb 2 $ "CANNOT REFINE ABSTRACT CALL TYPE OF FUNCTION " ++ snd qf
    addConditionRestriction qf cond

-- Adds an info about cases with missing branches in the current function.
addMissingCase :: TermDomain a => Expr -> [QName] -> VerifyM a ()
addMissingCase exp qcs = do
  st <- get
  (qf,ar,_) <- getCurrentFunc
  put $
    st { vstPartialBranches = union [(qf,ar,exp,qcs)] (vstPartialBranches st) }
  addCallTypeRestriction qf failACallType

-- Sets the types and expressions for variables.
getVarExps :: TermDomain a => VerifyM a [(Int,TypeExpr,Expr)]
getVarExps = fmap vstVarExp get

-- Sets the types and expressions for variables.
setVarExps :: TermDomain a => [(Int,TypeExpr,Expr)] -> VerifyM a ()
setVarExps varexps = do
  st <- get
  put $ st { vstVarExp = varexps }

-- Sets the FlatCurry type of a given variable.
-- This is used to specialize numeric types during verification
-- of predefined numeric operations.
setVarExpTypeOf :: TermDomain a => Int -> TypeExpr -> VerifyM a ()
setVarExpTypeOf var te = do
  st <- get
  put $ st { vstVarExp = map (\(v,t,e) -> if v==var then (v,te,e) else (v,t,e))
                             (vstVarExp st) }

-- Adds types and expressions for new variables.
addVarExps :: TermDomain a => [(Int,TypeExpr,Expr)] -> VerifyM a ()
addVarExps varexps = do
  st <- get
  put $ st { vstVarExp = vstVarExp st ++ varexps }

-- Get all currently stored variable types.
getVarTypes :: TermDomain a => VerifyM a (VarTypesMap a)
getVarTypes = fmap vstVarTypes get

-- Gets the currently stored types for a given variable.
getVarTypeOf :: TermDomain a => Int -> VerifyM a (VarTypes a)
getVarTypeOf v = do
  st <- get
  return $ maybe [] id (lookup v (vstVarTypes st))

-- Sets all variable types.
setVarTypes :: TermDomain a => VarTypesMap a -> VerifyM a ()
setVarTypes vartypes = do
  st <- get
  put $ st { vstVarTypes = vartypes }

-- Adds a new variable type to the current set of variable types.
-- It could be an alternative type for an already existent variable or
-- a type for a new variable.
addVarType :: TermDomain a => Int -> VarTypes a -> VerifyM a ()
addVarType v vts = do
  st <- get
  put $ st { vstVarTypes = addVarType2Map v vts (vstVarTypes st) }

-- Adding multiple variable types:
addVarTypes :: TermDomain a => VarTypesMap a -> VerifyM a ()
addVarTypes vtsmap = do
  st <- get
  put $ st { vstVarTypes = concVarTypesMap (vstVarTypes st) vtsmap }

-- Adds a new variable `Any` type to the current set of variable types.
addVarAnyType :: TermDomain a => Int -> VerifyM a ()
addVarAnyType v = addVarType v (ioVarType anyType)

-- Removes an `Any` type for a given variable from the current
-- set of variable types.
-- Used to remove the initial `Any` types in let bindings.
removeVarAnyType :: TermDomain a => Int -> VerifyM a ()
removeVarAnyType v = do
  st <- get
  let vtsmap = vstVarTypes st
      vtsmap' = maybe vtsmap
                      (\vts -> setVarTypeInMap v
                                               (filter (not . isAnyIOType) vts)
                                               vtsmap)
                      (lookup v vtsmap)
  put $ st { vstVarTypes = vtsmap' }
 where
  isAnyIOType (vt,vs) =
    case (vt,vs) of (IOT [([], at)], []) -> isAnyType at
                    _                    -> False

-- Gets current branch condition in internal representation.
getCondition :: TermDomain a => VerifyM a (Expr -> Expr)
getCondition = fmap vstCondition get

-- Gets the expanded current branch condition.
getExpandedCondition :: TermDomain a => VerifyM a Expr
getExpandedCondition = do
  st <- get
  return $ expandExpr (vstVarExp st) (vstCondition st fcTrue)

-- Gets the expanded current branch condition where a conjunct has been added.
getExpandedConditionWithConj :: TermDomain a => Expr -> VerifyM a Expr
getExpandedConditionWithConj conj = do
  st <- get
  return $ expandExpr (vstVarExp st) (vstCondition st conj)

-- Sets the current branch condition in internal representation.
setCondition :: TermDomain a => (Expr -> Expr) -> VerifyM a ()
setCondition expf = do
  st <- get
  put $ st { vstCondition = expf }

-- Sets the initial condition for a function call.
setCallCondition :: TermDomain a => Expr -> VerifyM a ()
setCallCondition exp = do
  st <- get
  put $ st { vstCondition = fcAnd exp }

-- Adds a conjunct to the current condition.
addConjunct :: TermDomain a => Expr -> VerifyM a ()
addConjunct exp = do
  st <- get
  put $ st { vstCondition = \c -> (vstCondition st) (fcAnd exp c) }

-- Adds to the current condition an equality between a variable and
-- a constructor applied to a list of fresh pattern variables.
-- This condition is expressed as a case expression where
-- the hole of the current condition is in the branch for the given
-- constructor and all alternative branches return a `False` value.
addSingleCase :: TermDomain a => Int -> QName -> [Int] -> VerifyM a ()
addSingleCase casevar qc branchvars = do
  st <- get
  let siblings    = siblingsOfCons (M.fromList (vstConsInfos st)) qc
      catchbranch = if null siblings then []
                                     else [Branch (Pattern anonCons []) fcFalse]
  put $ st { vstCondition =
               \c -> (vstCondition st)
                       (Case Rigid (Var casevar)
                          ([Branch (Pattern qc branchvars) c] ++ catchbranch)) }

-- Adds an equality between a variable and an expression as a conjunct
-- to the current condition.
addEquVarCondition :: TermDomain a => Int -> Expr -> VerifyM a ()
addEquVarCondition var exp = do
  let conj = if exp == fcTrue
               then Var var
               else if exp == fcFalse
                      then fcNot (Var var)
                      else Comb FuncCall (pre "==") [Var var, exp]
                           -- note: the Eq class dictioniary is missing...
  addConjunct conj

-- Gets the possible non-fail condition of a given operation.
getNonFailConditionOf :: TermDomain a => QName -> VerifyM a (Maybe NonFailCond)
getNonFailConditionOf qf = (>>= viNonFailCond) . flip funcInfoFromEnv qf <$> askVFuncEnv

-- Gets the abstract call type of a given operation.
-- The trivial abstract call type is returned for encapsulated search operations.
getCallType :: TermDomain a => QName -> Int -> VerifyM a (ACallType a)
getCallType qf ar
  | isEncSearchOp qf || isSetFunOp qf
  = return trivialACallType
  | otherwise
  = do
  st <- get
  env <- askVFuncEnv
  return $
    if qf == pre "error" && optError (vstToolOpts st)
      then failACallType
      else maybe (trace ("Warning: call type of operation " ++ show qf ++ " not found!") trivialACallType) id (funcInfoFromEnv env qf >>= viCallType)
 where
  trivialACallType = Just $ take ar (repeat anyType)

-- Gets the in/out type for an operation of a given arity.
-- If the operation is not found, returns a general type.
-- The trivial in/out type is returned for encapsulated search operations.
getFuncType :: TermDomain a => QName -> Int -> VerifyM a (InOutType a)
getFuncType qf ar
  | isEncSearchOp qf || isSetFunOp qf
  = return $ trivialInOutType ar
  | otherwise
  = do env <- askVFuncEnv
       maybe (do liftIO $ printInfoLine $
                   "WARNING: in/out type of '" ++ show qf ++ "' not found!"
                 return $ trivialInOutType ar)
             return
             (funcInfoFromEnv env qf >>= viIOType)

-- Increment number of checks of non-trivial function calls.
incrNonTrivialCall :: TermDomain a => VerifyM a ()
incrNonTrivialCall = do
  st <- get
  put $ st { vstStats = (\ (f,c,s) -> (f+1,c,s)) (vstStats st) }

-- Increment number of checks of incomplete case expressions.
incrIncompleteCases :: TermDomain a => VerifyM a ()
incrIncompleteCases = do
  st <- get
  put $ st { vstStats = (\ (f,c,s) -> (f,c+1,s)) (vstStats st) }

-- Increment number of checks of unsatisfiability with SMT.
incrUnsatSMT :: TermDomain a => VerifyM a ()
incrUnsatSMT = do
  st <- get
  put $ st { vstStats = (\ (f,c,s) -> (f,c,s+1)) (vstStats st) }

-- Gets the tool options from the current state.
getToolOptions :: TermDomain a => VerifyM a Options
getToolOptions = get >>= return . vstToolOpts

--- Prints a string with `printInfoString` if the verbosity is at least as
--- the given one.
printIfVerb :: TermDomain a => Int -> String -> VerifyM a ()
printIfVerb v s = do
  opts <- getToolOptions
  when (optVerb opts >= v) $ liftIO $ printInfoString s

-- Verify a FlatCurry function declaration.
verifyFunc :: TermDomain a => FuncDecl -> VerifyM a ()
verifyFunc (Func qf _ _ ftype rule) = case rule of
  Rule vs exp -> unless noVerify $ do
                   verifyFuncRule vs ftype (normalizeLet exp)
  External _  -> return ()
 where
  noVerify = qf `elem` noVerifyFunctions ||
             nonfailSuffix `isSuffixOf` snd qf

-- A list of operations that do not need to be verified.
-- These are operations that are non-failing but this property
-- cannot be ensured by the current tool.
noVerifyFunctions :: [QName]
noVerifyFunctions =
  [ pre "aValueChar" -- is non-failing if minBound <= maxBound, which is True
  ]

verifyFuncRule :: TermDomain a => [Int] -> TypeExpr -> Expr -> VerifyM a ()
verifyFuncRule vs ftype exp = do
  setFreshVarIndex (maximum (0 : vs ++ allVars exp) + 1)
  setVarExps  (map (\(v,te) -> (v, te, Var v)) (funcType2TypedVars vs ftype))
  qf <- getCurrentFuncName
  mbnfcond <- getNonFailConditionOf qf
  maybe
    (setCallCondition fcTrue)
    (\(nfcvars,fcond) -> do
      let freenfcargs = filter ((`notElem` vs) . fst) nfcvars
      newfvars <- mapM (\_ -> newFreshVarIndex) freenfcargs
      -- add renamed free variables of condition to the current set of variables
      addVarExps (map (\(v,(_,t)) -> (v,t,Var v)) (zip newfvars freenfcargs))
      -- rename free variables before adding the condition:
      setCallCondition $ expandExpr (map (\(nv,(v,t)) -> (v,t,Var nv))
                                         (zip newfvars freenfcargs))  fcond)
     mbnfcond
  printIfVerb 3 $ "CHECKING FUNCTION " ++ snd qf
  ctype    <- getCallType qf (length vs)
  rhstypes <- mapM (\f -> getCallType f 0) (funcsInExpr exp)
  if all isTotalACallType (ctype:rhstypes)
    then printIfVerb 3 $ "not checked since trivial"
    else maybe (maybe
                  (printIfVerb 3 "not checked since marked as always failing")
                  (\_ -> do
                    setVarTypes (map (\v -> (v, [(IOT [([], anyType)], [])]))
                                     vs)
                    showVarExpTypes
                    verifyExpr True exp
                    return () )
                  mbnfcond)
               (\atargs -> do
                  setVarTypes (map (\(v,at) -> (v, [(IOT [([], at)], [])]))
                                   (zip vs atargs))
                  showVarExpTypes
                  verifyExpr True exp
                  return ())
               ctype
  printIfVerb 3 $ take 70 (repeat '-')

-- Shows the current variable expressions and types if verbosity > 2.
showVarExpTypes :: TermDomain a => VerifyM a ()
showVarExpTypes = do
  qf <- getCurrentFuncName
  opts <- getToolOptions
  when (optVerb opts > 3) $ do
    st <- get
    liftIO $ printInfoString $
      "Current set of variables in function " ++ snd qf ++
      ":\nVariable bindings:\n" ++
      unlines (map (\ (v,te,e) -> showBindExp v e ++
                     if te == unknownType then "" else " :: " ++ showTypeExp te)
                   (vstVarExp st))
    vartypes <- getVarTypes
    liftIO $ printInfoString $ "Variable types\n" ++ showVarTypes vartypes
    cond <- getExpandedCondition
    liftIO $ printInfoLine $ "Current condition: " ++ showSimpExp cond

-- Verify an expression (if the first argument is `True`) and,
-- if the expression is not a variable, create a fresh
-- variable and a binding for this variable.
-- The variable which identifies the expression is returned.
verifyExpr :: TermDomain a => Bool -> Expr -> VerifyM a Int
verifyExpr verifyexp exp = case exp of
  Var v -> do iots <- if verifyexp then verifyVarExpr v exp
                                   else return [(v, ioVarType anyType)]
              addVarTypes iots
              return v
  _     -> do v <- newFreshVarIndex
              addVarExps [(v, unknownType, exp)]
              iots <- if verifyexp then verifyVarExpr v exp
                                   else return [(v, ioVarType anyType)]
              addVarTypes iots
              return v

-- Verify an expression identified by variable (first argument).
-- The in/out variable types collected for the variable are returned.
verifyVarExpr :: TermDomain a => Int -> Expr -> VerifyM a (VarTypesMap a)
verifyVarExpr ve exp = case exp of
  Var v         -> if v == ve
                     then return []
                     else do
                       --lift $ printInfoLine $
                       --  "Expression with different vars: " ++ show (v,ve)
                       --showVarExpTypes
                       vtypes <- getVarTypeOf v
                       -- TODO: improve by handling equality constraint v==ve
                       -- instead of copying the current types for v to ve:
                       return $ [(ve, vtypes)]
  Lit l         -> return [(ve, [(IOT [([], aLit l)], [])])]
  Comb ct qf es -> checkPredefinedOp exp $ do
    vs <- if isEncSearchOp qf
            then -- for encapsulated search, failures in arguments are hidden
                 mapM (verifyExpr False) es
            else if isSetFunOp qf
                   then -- for a set function, the function argument is hidden
                        mapM (\ (i,e) -> verifyExpr (i>0) e)
                             (zip [0..] es)
                   else mapM (verifyExpr True) es
    case ct of
      FuncCall -> do verifyFuncCall exp qf vs
                     ftype <- getFuncType qf (length vs)
                     return [(ve, [(ftype, vs)])]
      FuncPartCall n -> -- note: also partial calls are considered as constr.
                  do ctype <- getCallType qf (n + length es)
                     unless (isTotalACallType ctype) $ do
                       printIfVerb 3 $ "UNSATISFIED ABSTRACT CALL TYPE: " ++
                         "partial application of non-total function\n"
                       addFailedFunc exp Nothing fcTrue
                     -- note: also partial calls are considered as constructors
                     returnConsIOType qf vs ve
      _        -> returnConsIOType qf vs ve
  Let bs e      -> do addVarExps (map (\(v,be) -> (v, unknownType, be)) bs)
                      mapM_ (addVarAnyType . fst) bs
                      iotss <- mapM (\ (v,be) -> verifyVarExpr v be) bs
                      -- remove initially set anyType's for the bound vars:
                      mapM_ (removeVarAnyType . fst) bs
                      addVarTypes (concat iotss)
                      mapM_ (addAnyTypeIfUnknown . fst) bs
                      verifyVarExpr ve e
  Free vs e     -> do addVarExps (map (\v -> (v, unknownType, Var v)) vs)
                      mapM_ addVarAnyType vs
                      verifyVarExpr ve e
  Or e1 e2      -> do iots1 <- verifyVarExpr ve e1 -- 
                      iots2 <- verifyVarExpr ve e2
                      return (concVarTypesMap iots1 iots2)
  Case _ ce bs  -> do cv <- verifyExpr True ce
                      verifyMissingBranches exp cv bs
                      iotss <- mapM (verifyBranch cv ve) bs
                      return (foldr concVarTypesMap [] iotss)
  Typed e _     -> verifyVarExpr ve e -- TODO: use type info
 where
  -- adds Any type for a variable if it is unknown
  addAnyTypeIfUnknown v = do
    vts <- getVarTypeOf v
    when (null vts) (addVarAnyType v)

  -- Return an input/output type for a constructor and its arguments
  returnConsIOType qc vs rv = do
    vts <- getVarTypes
    let vstypes = map (flip getVarType vts) vs
    --let anys = anyTypes (length vs)  -- TODO: use vs types from VarTypes!!!!
    --return [(rv, IOT [(anys, aCons qc anys)], vs)]
    return [(rv, [(IOT [(vstypes, aCons qc vstypes)], vs)])]

-- Verify the abstract type or non-fail condition of a function call.
-- The second argument is the function call as a FlatCurry expression,
-- the third argument is the function name, and the fourth argument
-- are the argument variables.
verifyFuncCall :: TermDomain a => Expr -> QName -> [Int] -> VerifyM a ()
verifyFuncCall exp qf vs = do
  opts <- fmap vstToolOpts get
  if qf == pre "failed" || (optError opts && qf == pre "error")
    then do
      bcond  <- getExpandedCondition
      unsat  <- incrNonTrivialCall >> incrUnsatSMT >> isUnsatisfiable bcond
      if unsat
        then do currfn <- getCurrentFuncName
                printIfVerb 3 $ "FUNCTION " ++ snd currfn ++ ": CALL TO " ++
                  snd qf ++ showArgumentVars vs ++ " NOT REACHABLE\n"
        else addFailedFunc exp Nothing fcTrue
    else do atype <- getCallType qf (length vs)
            if isTotalACallType atype 
              then return ()
              else do mbnfcond <- getNonFailConditionOf qf
                      verifyNonTrivFuncCall exp qf vs atype mbnfcond

-- Verify the non-trivial abstract type or non-fail condition
--  of a function call.
-- The first argument is the expression of the call (used to show
-- the possible failed expression), the second and third arguments
-- are the name and the arguments of the current function call.
-- The last argument is the possible non-fail condition for this function.
verifyNonTrivFuncCall :: TermDomain a => Expr -> QName -> [Int]
                      -> ACallType a -> Maybe NonFailCond -> VerifyM a ()
verifyNonTrivFuncCall exp qf vs atype mbnfcond = do
  incrNonTrivialCall
  currfn <- getCurrentFuncName
  printIfVerb 3 $ "FUNCTION " ++ snd currfn ++ ": VERIFY CALL TO " ++
                  snd qf ++ showArgumentVars vs ++
                  "\n  w.r.t. call type: " ++ prettyFunCallAType atype
  callcond <- maybe
    (return fcFalse)
    (\(nfcvars,nfcond) -> do
      -- compute the precondition for this call by renaming the arguments:
      let freenfcargs = filter ((`notElem` [1..length vs]) . fst) nfcvars
      newfvars <- mapM (\_ -> newFreshVarIndex) freenfcargs
      -- add renamed free variables of condition to the current set of variables
      addVarExps (map (\(v,(_,t)) -> (v,t,Var v)) (zip newfvars freenfcargs))
      -- rename variable in condition:
      let rnmcvars = zip [1.. length vs] vs ++
                     map (\(nv,(v,_)) -> (v, nv)) (zip newfvars freenfcargs)
      st <- get
      let callcond0 = expandExpr (vstVarExp st) (renameAllVars rnmcvars nfcond)
      printIfVerb 3 $ "  and call condition: " ++ showSimpExp callcond0
      return callcond0)
    mbnfcond
  showVarExpTypes
  --allvts <- getVarTypes
  --printIfVerb 3 $ "Current variable types:\n" ++ showVarTypes allvts
  svts <- fmap simplifyVarTypes getVarTypes
  printIfVerb 4 $ "Simplified variable types:\n" ++ showVarTypes svts
  let vts = map (\v -> (v, getVarType v svts)) vs
  printIfVerb 3 $ "Variable types in this call: " ++ printVATypes vts
  if subtypeOfRequiredCallType (map snd vts) atype
    then printIfVerb 3 "CALL TYPE SATISFIED\n"
    else -- Check whether types of call argument variables can be made
         -- more specific to satisfy the call type. If all these variables
         -- are parameters of the current functions, specialize the
         -- call type of this function and analyze it again.
         do printIfVerb 3 "UNSATISFIED ABSTRACT CALL TYPE\n"
            maybe
              (if callcond == fcFalse
                 then addFailedFunc exp Nothing callcond
                 else do
                  -- check whether the negated call condition is unsatisfiable
                  -- (if yes: call condition holds)
                  implcond <- getExpandedConditionWithConj (fcNot callcond)
                  implied  <- incrUnsatSMT >> isUnsatisfiable implcond
                  if implied
                    then printIfVerb 3 "CALL CONDITION SATISFIED\n"
                    else addFailedFunc exp Nothing callcond)
              (\newvts -> do
                 printIfVerb 3 $ "COULD BE SATISFIED BY ENSURING:\n" ++
                                 printVATypes newvts
                 addFailedFunc exp (Just newvts) fcTrue
              )
              (specializeToRequiredType vts atype)
 where
  printVATypes = intercalate ", " . map (\ (v,t) -> show v ++ '/' : showType t)

-- Auxiliary operation to support specific handling of applying predefined
-- operations with specific non-fail conditions.
-- If the current expression (first argument) is not a specific
-- predefined operation, the computation will be continued by calling
-- the second argument (a continuation).
-- For instance, division operations require that the second argument is
-- non-zero. If this argument is a constant, it will be immediately checked,
-- otherwise a non-fail condition will be generated and checked.
-- Since such primitive operations can be invoked by FlatCurry expressions
-- with various structures (due to the numeric type classes),
-- these structures will be checked.
checkPredefinedOp :: TermDomain a => Expr -> VerifyM a (VarTypesMap a)
                  -> VerifyM a (VarTypesMap a)
checkPredefinedOp exp cont = case exp of
  -- check integer division:
  Comb FuncCall ap1 [Comb FuncCall ap2 [Comb FuncCall qf _, arg1], arg2]
    | ap1 == apply && ap2 == apply && qf `elem` intDivOps
    -> tryCheckNumValue (intValue arg2) qf arg1 arg2 fcInt
  -- check float division:
  Comb FuncCall qf [arg1, arg2]
    | qf == pre "_impl#/#Prelude.Fractional#Prelude.Float#"
    -> tryCheckNumValue (floatValue arg2) qf arg1 arg2 fcFloat
  Comb FuncCall ap1 [Comb FuncCall ap2 [Comb FuncCall qf _, arg1], arg2]
    | ap1 == apply && ap2 == apply && qf `elem` floatDivOps
    -> tryCheckNumValue (floatValue arg2) qf arg1 arg2 fcFloat
  -- check sqrt call:
  Comb FuncCall ap1 [Comb FuncCall qf [], arg]
    | ap1 == apply && qf == pre "_impl#sqrt#Prelude.Floating#Prelude.Float#"
    -> maybe (verifyPredefinedOp qf [arg] fcFloat)
             (\x -> do unless (x >= 0) $ addFailedFunc exp Nothing fcFalse
                       return [])
             (floatValue arg)
  _ -> cont
 where
  tryCheckNumValue mbx qf arg1 arg2 te =
    maybe (verifyPredefinedOp qf [arg1,arg2] te)
          (\z -> do if z == 0 then addFailedFunc exp Nothing fcFalse
                              else printIfVerb 4 nonZeroOkMsg
                    verifyExpr True arg1
                    return [])
          mbx

  verifyPredefinedOp qf args te =
     maybe (do printIfVerb 0 $
                 "WARNING: non-fail condition of " ++ snd qf ++ " not found!"
               cont)
           (\nfc -> do vs <- mapM (verifyExpr True) args
                       mapM_ (\v -> setVarExpTypeOf v te) vs
                       verifyNonTrivFuncCall exp qf vs failACallType (Just nfc)
                       return [])
           (lookupPredefinedNonFailCond qf)

  intValue e = case e of
    Lit (Intc i) -> Just i
    Comb FuncCall ap [ Comb FuncCall fromint _ , nexp] 
      | ap == apply && fromint == pre "fromInt" -> intValue nexp
    _            -> Nothing

  floatValue e = case e of
    Lit (Floatc f) -> Just f
    Comb FuncCall ap [ Comb FuncCall fromfloat _ , nexp] 
      | ap == apply && fromfloat == pre "fromFloat" -> floatValue nexp
    Comb FuncCall ap [(Comb FuncCall fromint _), nexp]
       | ap == apply && fromint == pre "fromInt" -> fmap fromInt (intValue nexp)
    Comb FuncCall negFloat [fexp]
      | negFloat == pre "_impl#negate#Prelude.Num#Prelude.Float#"
                   -> fmap negate (floatValue fexp)
    _              -> Nothing

  apply = pre "apply"

  nonZeroOkMsg = "Divion with non-zero constant is non-failing"

-- Verify whether missing branches exists and are not reachable.
verifyMissingBranches :: TermDomain a => Expr -> Int -> [BranchExpr] -> VerifyM a ()
verifyMissingBranches _ _ [] = do
  currfn <- getCurrentFuncName
  error $ "Function " ++ snd currfn ++ " contains case with empty branches!"
verifyMissingBranches exp casevar (Branch (LPattern lit) _ : bs) = do
  incrIncompleteCases
  currfn <- getCurrentFuncName
  let lits = lit : map (patLiteral . branchPattern) bs
  cvtype <- getVarTypes >>= return . getVarType casevar
  unless (isSubtypeOf cvtype (foldr1 lubType (map aLit lits))) $ do
    printIfVerb 3 $ showIncompleteBranch currfn exp [] ++ "\n"
    showVarExpTypes
    addMissingCase exp []
verifyMissingBranches exp casevar (Branch (Pattern qc _) _ : bs) = do
  consinfos <- getConsInfos
  let otherqs  = map ((\p -> (patCons p, length(patArgs p))) . branchPattern) bs
      siblings = siblingsOfCons (M.fromList consinfos) qc
      missingcs = siblings \\ otherqs -- constructors having no branches
  currfn <- getCurrentFuncName
  unless (null missingcs) $ do
    printIfVerb 0 $ -- just for checking in order to refactor in the future...
      "MISSING CONSTRUCTORS " ++ show missingcs ++ " IN FUNCTION " ++ snd currfn
    incrIncompleteCases
    cvtype <- getVarTypes >>= return . getVarType casevar
    let posscs = map fst
                     (filter (\(c,ar) -> let ctype = aCons c (anyTypes ar)
                                         in joinType cvtype ctype /= emptyType)
                             missingcs)
    cond <- getExpandedCondition
    unless (null posscs) $
      if cond == fcTrue
        then do
          printIfVerb 3 $ showIncompleteBranch currfn exp posscs ++ "\n"
          showVarExpTypes
          addMissingCase exp posscs
        else do
          showVarExpTypes
          unsatcons <- fmap concat $ mapM checkMissCons posscs
          unless (null unsatcons) $ do
            printIfVerb 3 $
              "UNCOVERED CONSTRUCTORS: " ++ unwords (map snd unsatcons)
            setNewFunCondition currfn nfcFalse
            addMissingCase exp unsatcons
 where
  -- check whether a constructor is excluded by the current call condition:
  checkMissCons cs = do
    printIfVerb 4 $ "CHECKING UNREACHABILITY OF CONSTRUCTOR " ++ snd cs
    consinfos <- getConsInfos
    let iscons = transTester (M.fromList consinfos) cs (Var casevar)
    bcond <- getExpandedCondition
    unsat <- isUnsatisfiable (fcAnd iscons bcond)
    return $ if unsat then [] else [cs]

-- Shows a message about an incomplete branch.
-- If the third argument is the empty list, it is a literal branch.
showIncompleteBranch :: QName -> Expr -> [QName] -> String
showIncompleteBranch qf e cs@(_:_) =
  "Function '" ++ snd qf ++ "': constructor" ++
  (if length cs > 1 then "s" else "") ++ " '" ++
  unwords (map snd cs) ++ "' " ++
  (if length cs > 1 then "are" else "is") ++ " not covered in:\n" ++
  showExp e
showIncompleteBranch qf e [] =
  "Function '" ++ snd qf ++ "': the case on literals might be incomplete:\n" ++
  showExp e

-- Gets the state information which might be changed during branch verification.
getBranchState :: TermDomain a => VerifyM a ([(Int,TypeExpr,Expr)], VarTypesMap a, Expr -> Expr)
getBranchState = do
  ves  <- getVarExps
  vts  <- getVarTypes
  cond <- getCondition
  return (ves,vts,cond)

-- Gets the state information which might be changed during branch verification.
restoreBranchState :: TermDomain a =>
  ([(Int,TypeExpr,Expr)], VarTypesMap a, Expr -> Expr) -> VerifyM a ()
restoreBranchState (ves,vts,cond) = do
  setVarExps ves
  setVarTypes vts
  setCondition cond

-- Verify a branch where the first argument is the case argument variable
-- and the second argument is the variable identifying the case expression.
verifyBranch :: TermDomain a => Int -> Int -> BranchExpr
             -> VerifyM a (VarTypesMap a)
verifyBranch casevar ve (Branch (LPattern l) e) = do
  bstate <- getBranchState
  vts  <- getVarTypes
  let branchvartypes = bindVarInIOTypes casevar (aLit l) vts
  printIfVerb 4 $ "BRANCH WITH LITERAL '" ++ show l ++ "'"
  addEquVarCondition casevar (Lit l)
  if isEmptyType (getVarType casevar branchvartypes)
    then return [] -- unreachable branch
    else do setVarTypes branchvartypes
            iots <- verifyVarExpr ve e
            restoreBranchState bstate
            return iots
verifyBranch casevar ve (Branch (Pattern qc vs) e) = do
  bstate <- getBranchState
  ves  <- getVarExps
  consinfos <- getConsInfos
  let vet = maybe unknownType snd3 (find ((== casevar) . fst3) ves)
  addVarExps (map (\ (v,vt) -> (v, vt, Var v))
                  (zip vs (if vet == unknownType
                             then repeat unknownType
                             else patArgTypes consinfos vet)))
  vts  <- getVarTypes
  let pattype        = aCons qc (anyTypes (length vs))
      branchvartypes = simplifyVarTypes (bindVarInIOTypes casevar pattype vts)
      casevartype    = getVarType casevar branchvartypes
  -- add single case for case variable and pattern to the current condition:
  if null vs then addEquVarCondition casevar (Comb ConsCall qc [])
             else addSingleCase casevar qc vs
  printIfVerb 4 $ "BRANCH WITH CONSTRUCTOR '" ++ snd qc ++ "'"
  showVarExpTypes
  if isEmptyType casevartype
    then do restoreBranchState bstate
            return [] -- unreachable branch
    else do setVarTypes branchvartypes
            mapM_ (\(v,t) -> addVarType v (ioVarType t))
                  (zip vs (argTypesOfCons qc (length vs) casevartype))
            iots <- verifyVarExpr ve e
            restoreBranchState bstate
            return iots
 where
  -- compute the argument types from the result type `pt` of the branch variable
  -- where some regularly occurring constructors are directly implemented
  patArgTypes consinfos pt
    | qc == pre ":"
    = case pt of TCons tc [et] | tc == pre "[]" -> [et, pt]
                 _                              -> noPatTypeErr
    | qc == pre "(,)"
    = case pt of TCons tc [t1,t2] | tc == pre "(,)" -> [t1,t2]
                 _                                  -> noPatTypeErr
    | qc == pre "Just"
    = case pt of TCons tc [t] | tc == pre "Maybe" -> [t]
                 _                                -> noPatTypeErr
    | otherwise
    = maybe (error $ "Info about constructor '" ++ snd qc ++ "' not found!")
            (\(_,ConsType tes tc tvs,_) ->
              case pt of
                TCons dtc ats | tc == dtc -> map (applyTSubst (zip tvs ats)) tes
                _                         -> noPatTypeErr
            )
            (lookup qc consinfos)

  noPatTypeErr = error $
    "verifyBranch: cannot compute pattern argument types for " ++ snd qc

-- Gets the abstract type of a variable w.r.t. a set of variable types.
getVarType :: TermDomain a => Int -> VarTypesMap a -> a
getVarType v vtsmap =
  maybe (error $ "Type of variable " ++ show v ++ " not found!")
        (\vts -> let rts = concatMap (\ (IOT iots, _) -> map snd iots) vts 
                 in if null rts then emptyType
                                else foldr1 lubType rts)
        (lookup v vtsmap)

------------------------------------------------------------------------------
--- Computes for a given set of function declarations in a module
--- a mapping from module function names to the list of function
--- declarations where these names are used in the right-hand side.
funcDecls2Usage :: String -> [FuncDecl] -> M.Map QName [FuncDecl]
funcDecls2Usage mname fdecls = addFDecls (M.empty) fdecls
 where
  addFDecls m []       = m
  addFDecls m (fd:fds) =
    let rhsfuns = filter (\f -> fst f == mname) (usedFuncsInFunc fd)
    in M.insertListWith unionFDecls (map (\qf -> (qf,[fd])) rhsfuns)
                          (addFDecls m fds)

unionFDecls :: [FuncDecl] -> [FuncDecl] -> [FuncDecl]
unionFDecls = unionBy (\fd1 fd2 -> funcName fd1 == funcName fd2)

--- Get function names used in the right-hand side of a function declaration.
usedFuncsInFunc :: FuncDecl -> [QName]
usedFuncsInFunc = usedFuncsInRule . funcRule

--- Get function names used in the right-hand side of a rule.
usedFuncsInRule :: Rule -> [QName]
usedFuncsInRule = trRule (\_ body -> funcsInExpr body) (\_ -> [])

------------------------------------------------------------------------------
--- A list of any types of a given length.
anyTypes :: TermDomain a => Int -> [a]
anyTypes n = take n (repeat anyType)

------------------------------------------------------------------------------
-- Utilities

-- I/O action to force evaluation of the argument to normal form.
enforceNormalForm :: Options -> String -> a -> IO ()
enforceNormalForm opts s x
  | optEnforceNF opts
  = do whenStatus opts $
         printInfoString $ "EVALUATE " ++ s ++ " TO NORMAL FORM..."
       (id $!! x) `seq` return ()
       printWhenStatus opts "DONE"
  | otherwise
  = return ()

------------------------------------------------------------------------------
-- Shows the simplified expression.
showSimpExp :: Expr -> String
showSimpExp = showExp . simpExpr

--- Checks the unsatisfiability of a Boolean expression w.r.t. a set
--- of variables (second argument) with an SMT solver.
isUnsatisfiable :: TermDomain a => Expr -> VerifyM a Bool
isUnsatisfiable bexp = do
  st <- get
  if optSMT (vstToolOpts st)
    then do
      fname  <- getCurrentFuncName
      vts <- fmap (map (\(v,te,_) -> (v,te))) getVarExps
      let allvs    = allFreeVars bexp
      let vtypes   = filter ((`elem` allvs) . fst) vts
          question = "Verifying function " ++ snd fname ++ ":\n\n" ++
                     "IS\n  " ++ showSimpExp bexp ++ "\nUNSATISFIABLE?"
      unless (all (`elem` map fst vtypes) allvs) $ liftIO $ printInfoLine $
        "WARNING in operation '" ++ snd fname ++
        "': missing variables in unsatisfiability check!"
      consinfos <- getConsInfos
      -- TODO: Port SMT handling directly to the verification framework
      -- (we'd have to figure out a way to keep the legacy implementation intact
      -- or just drop the legacy flag entirely)
      answer <- withLegacyProgInfo $ \pistore -> do
        liftIO $ checkUnsatisfiabilityWithSMT (vstToolOpts st)
                         fname question pistore (M.fromList consinfos) vtypes bexp
      maybe (setToolError >> return False) return answer
    else return False

------------------------------------------------------------------------------
