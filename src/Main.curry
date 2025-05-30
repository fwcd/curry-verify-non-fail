-------------------------------------------------------------------------
--- A tool to verify Curry programs w.r.t. failing computations.
--- Thus, a program successfully verified by this tool should never
--- fail at run-time (apart from explicit error) provided that
--- the call types are satisfied when invoking a function.
---
--- @author Michael Hanus
--- @version May 2025
-------------------------------------------------------------------------

module Main ( main ) where

import Control.Monad               ( unless, when )
import Curry.Compiler.Distribution ( curryCompiler )
import Data.Char                   ( toLower )
import Data.IORef
import Data.List
import Data.Maybe                  ( isNothing )
import System.Environment          ( getArgs )

import Debug.Trace ( trace )

-- Imports from dependencies:
import Analysis.Types             ( Analysis, analysisName, startValue )
import Analysis.TermDomain
import Analysis.Values
import Control.Monad.Trans.Class  ( lift )
import Control.Monad.Trans.State  ( StateT, get, put, execStateT )
import qualified Data.Map as Map
import qualified Data.Set as Set
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
import XML

-- Imports from package modules:
import FlatCurry.Build
import FlatCurry.Simplify         ( simpExpr )
import Verify.CallTypes
import Verify.Files
import Verify.Helpers
import Verify.IOTypes
import Verify.NonFailConditions
import Verify.Options
import Verify.ProgInfo
import Verify.Statistics
import Verify.WithSMT

------------------------------------------------------------------------------
banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "Curry Non-Failure Verifier (Version of 05/05/25)"
  bannerLine = take (length bannerText) (repeat '=')

main :: IO ()
main = do
  args <- getArgs
  (opts0,progs) <- processOptions banner args
  -- set analysis to top values if unspecified
  let opts = if null (optDomainID opts0)
               then opts0 { optDomainID = analysisName resultValueAnalysisTop }
               else opts0
  when (optDeleteCache opts0) $ deleteVerifyCacheDirectory opts0
  case progs of
    [] -> unless (optDeleteCache opts0) $ do
            printInfoLine "Module name missing!"
            printInfoLine "Try option '--help' for usage information."
            exitWith 1
    ms -> do
      if optDomainID opts == analysisName resultValueAnalysisTop
        then runWith resultValueAnalysisTop opts ms
        else
          if optDomainID opts == analysisName resultValueAnalysis2
            then runWith resultValueAnalysis2 opts ms
            else
              if optDomainID opts == analysisName resultValueAnalysis5
                then runWith resultValueAnalysis5 opts ms
                else error "Unknown analysis domain ID!"
 where
  runWith analysis opts ms = do
    printWhenStatus opts banner
    pistore <- newIORef emptyProgInfo
    astore  <- newIORef (AnaStore [])
    mapM_ (runModuleAction (verifyModuleIfNew analysis pistore astore opts)) ms

--- Verify a single module if necessary, i.e., not previously verified.
verifyModuleIfNew :: TermDomain a => Analysis a -> IORef ProgInfo
                  -> IORef (AnalysisStore a) -> Options -> String -> IO ()
verifyModuleIfNew valueanalysis pistore astore opts0 mname = do
  z3exists <- fileInPath "z3"
  let z3msg = "Option '--nosmt' activated since SMT solver Z3 not found in PATH!"
  opts <- if z3exists || not (optSMT opts0)
            then return opts0
            else do printInfoLine z3msg
                    return opts0 { optSMT = False }
  printWhenStatus opts $ "Processing module '" ++ mname ++ "':"
  flatprog <- getFlatProgFor pistore mname
  outdatedtypes <- typeFilesOutdated opts mname
  if outdatedtypes || optRerun opts || not (null (optFunction opts))
    then verifyModule valueanalysis pistore astore opts mname flatprog
    else do
      let fdecls       = progFuncs flatprog
          visfuncs     = filter (\fn -> optGenerated opts || isCurryID fn)
                                (map funcName
                                     (filter ((== Public) . funcVisibility)
                                             fdecls))
          visfuncset   = Set.fromList visfuncs
          isVisible qf = Set.member qf visfuncset
      let qname  = ("","")                  -- some (unused) QName
          avalue = startValue valueanalysis -- some (unused) abstract type value
      when (optIOTypes opts) $ do
        printWhenStatus opts "Reading in/out types from previous verification..."
        iotypes <- readIOTypes opts mname
        -- note: the `asTypeOf` expression is necessary to convince the type
        -- checker that `iotypes` is parametric over the type variable `a`:
        printIOTypes opts
          (filter (isVisible . fst)
                  (iotypes `asTypeOf` [(qname, valueInOutType avalue)]))
      printWhenStatus opts "Reading call types from previous verification..."
      (ctypes,nfconds) <- readCallCondTypes opts mname
      -- note: the `asTypeOf` expression is necessary to convince the type
      -- checker that `ctype` is parametric over the type variable `a`:
      printVerifyResults opts mname isVisible fdecls
        (ctypes `asTypeOf` [(qname, Just [avalue])]) nfconds
      return ()

--- Verify a single module.
verifyModule :: TermDomain a => Analysis a -> IORef ProgInfo
             -> IORef (AnalysisStore a) -> Options -> String -> Prog -> IO ()
verifyModule valueanalysis pistore astore opts mname flatprog = do
  let orgfdecls     = progFuncs flatprog
      numfdecls     = length orgfdecls
      visfuncs      = map funcName (filter ((== Public) . funcVisibility)
                                          orgfdecls)
      visfuncset    = Set.fromList visfuncs
      isVisible qf  = Set.member qf visfuncset
      showfuncset   = Set.fromList
                        (filter (\fn -> optGenerated opts || isCurryID fn)
                                visfuncs)
      isShowable qf = Set.member qf showfuncset
  imps@(impconsinfos,impacalltypes,impnftypes,impiotypes) <-
    if optImports opts
      then do
        let imports = progImports flatprog
        whenStatus opts $ printInfoString $
          "Reading abstract types of imports: " ++ unwords imports
        -- if only verification results should be shown, use quiet mode and
        -- do not show in/out types for imports:
        let impopts = if optVerb opts <= 1
                        then opts { optVerb = 0, optIOTypes = False }
                        else opts
        readTypesOfModules impopts
                           (verifyModuleIfNew valueanalysis pistore astore)
                           imports
      else return ([],[],[],[])
  if optTime opts then do whenStatus opts $ printInfoString "..."
                          (id $## imps) `seq` printWhenStatus opts "done"
                  else printWhenStatus opts ""
  let modconsinfos = consInfoOfTypeDecls (progTypes flatprog)
      consinfos = modconsinfos ++ impconsinfos
      -- verify function declarations with completed branches:
      fdecls  = map (completeBranchesInFunc consinfos False) orgfdecls
      funusage = funcDecls2Usage mname fdecls
  mtime <- getModuleModTime mname
  -- infer initial abstract call types:
  mboldacalltypes <- readCallTypeFile opts mtime mname
  (acalltypes, numntacalltypes, numpubacalltypes) <- id $!!  
    inferCallTypes opts consinfos isVisible mname mtime flatprog mboldacalltypes
  -- infer in/out types:
  iotypes <- id $!! inferIOTypes opts valueanalysis astore flatprog
  printIOTypesIfDemanded opts isShowable iotypes
  -- read previously inferred non-fail conditions:
  mbnfconds <- readNonFailCondFile opts mtime mname

  vstate <- initVerifyState pistore flatprog consinfos
                            (Map.fromList impacalltypes)
                            (Map.fromList impnftypes)
                            (Map.fromList acalltypes)
                            (Map.fromList (iotypes ++ impiotypes))
                            (maybe [] id mbnfconds) opts
  enforceNormalForm opts "VERIFYSTATE" vstate
  printWhenAll opts $ unlines $
    ("Function usage in module '" ++ mname ++ "':") :
    map (\ (qf, qfs) -> snd qf ++ ": used in " ++
                        unwords (map (snd . funcName) qfs))
        (Map.toList funusage)
  let withverify = optVerify opts &&
                   (null (optFunction opts) ||
                    isNothing mbnfconds || isNothing mboldacalltypes)
  (vnumits, vtime, vst) <-
   if withverify
     then do
       printWhenStatus opts $ "Start verification of '" ++ mname ++ "' (" ++
         show numfdecls ++ " functions):"
       pi1 <- getProcessInfos
       (numits,st) <- tryVerifyProg opts 0 vstate mname funusage fdecls
       pi2 <- getProcessInfos
       printVerifyResult opts st mname isShowable
       let tdiff = maybe 0 id (lookup ElapsedTime pi2) -
                   maybe 0 id (lookup ElapsedTime pi1)
       when (optTime opts && optVerb opts > 0) $ printInfoLine $
         "TOTAL VERIFICATION TIME: " ++ show tdiff ++ " msec"
       return (numits, tdiff, st)
     else return (0, 0, vstate)
  -- print statistics:
  let finalacalltypes   = Map.toList (vstCallTypes vst)
      finalntacalltypes = filter (not . isTotalACallType . snd) finalacalltypes
      ntiotypes         = filter (not . isAnyIOType . snd) iotypes
      numpubntiotypes   = length (filter (isShowable . fst) ntiotypes)
      numvisfuncs       = length visfuncs
      (stattxt,statcsv) = showStatistics opts vtime vnumits isShowable
                            numvisfuncs numfdecls
                            (numpubntiotypes, length ntiotypes)
                            (numpubacalltypes, numntacalltypes)
                            finalntacalltypes
                            (map fst (vstFunConds vst)) (vstStats vst)
  when (optStats opts) $ printInfoString stattxt
  when withverify $ do
    storeTypes opts mname fdecls modconsinfos finalacalltypes
      (filter (isVisible .fst) finalntacalltypes) (vstFunConds vst)
      (filter (isVisible . fst) iotypes)
    storeStatistics opts mname stattxt statcsv
    when (optStoreFuncs opts) $ do
      let nfname = mname ++ ".NONFAIL"
          flname = mname ++ ".FAIL"  
          totalfuncs = filter (isTotalACallType . snd) finalacalltypes
      writeFile nfname
        (unlines (map (\((mn,fn),_) -> mn ++ " " ++ fn) totalfuncs))
      printWhenStatus opts $
        "Non-failing functions stored in '" ++ nfname ++ "'."
      writeFile flname
        (unlines (map (\((mn,fn),_) -> mn ++ " " ++ fn) finalntacalltypes))
      printWhenStatus opts $
        "Possibly failing functions stored in '" ++ flname ++ "'."
  unless (null (optFunction opts)) $ showFunctionInfo opts mname vst
  when (vstError vst) $ exitWith 1

--- Infer the initial (abstract) call types of all functions in a program and
--- return them together with the number of all/public non-trivial call types.
--- The last argument are the already stored old call types, if they are
--- up to date.
inferCallTypes :: TermDomain a => Options -> [(QName,ConsInfo)]
               -> (QName -> Bool)
               -> String -> ClockTime -> Prog -> Maybe [(QName,ACallType a)]
               -> IO ([(QName, ACallType a)], Int, Int)
inferCallTypes opts consinfos isVisible mname mtime flatprog
               mboldacalltypes = do
  oldpubcalltypes <- readPublicCallTypeModule opts consinfos mtime mname
  let fdecls       = progFuncs flatprog
  let calltypes    = unionBy (\x y -> fst x == fst y) oldpubcalltypes
                             (map (callTypeFunc opts consinfos) fdecls)
      ntcalltypes  = filter (not . isTotalCallType . snd) calltypes
      pubcalltypes = filter (isVisible . fst) ntcalltypes
  if optVerb opts > 2
    then printInfoLine $ unlines $ "CONCRETE CALL TYPES OF ALL OPERATIONS:" :
           showFunResults prettyFunCallTypes calltypes
    else when (optVerb opts > 2 || optCallTypes opts) $
      printInfoLine $ unlines $
        ("NON-TRIVIAL CONCRETE CALL TYPES OF " ++
         (if optPublic opts then "PUBLIC" else "ALL") ++ " OPERATIONS:") :
        showFunResults prettyFunCallTypes
         (sortFunResults (if optPublic opts then pubcalltypes else ntcalltypes))

  let pubmodacalltypes = map (funcCallType2AType consinfos) oldpubcalltypes
      acalltypes = unionBy (\x y -> fst x == fst y) pubmodacalltypes
                           (maybe (map (funcCallType2AType consinfos) calltypes)
                                  id
                                  mboldacalltypes)
      ntacalltypes  = filter (not . isTotalACallType . snd) acalltypes
      pubacalltypes = filter (isVisible . fst) ntacalltypes
  if optVerb opts > 2
    then printInfoLine $ unlines $ "ABSTRACT CALL TYPES OF ALL OPERATIONS:" :
           showFunResults prettyFunCallAType acalltypes
    else when (optVerb opts > 2 || optCallTypes opts) $
      printInfoLine $ unlines $
        ("NON-TRIVIAL ABSTRACT CALL TYPES OF " ++
         (if optPublic opts then "PUBLIC" else "ALL") ++ " OPERATIONS:") :
        showFunResults prettyFunCallAType
          (sortFunResults $ if optPublic opts then pubacalltypes
                                              else ntacalltypes)
  return (acalltypes, length ntacalltypes, length pubacalltypes)

--- Infer the in/out types of all functions in a program and return them
--- together with the number of all and public non-trivial in/out types.
inferIOTypes :: TermDomain a => Options -> Analysis a -> IORef (AnalysisStore a)
             -> Prog -> IO [(QName, InOutType a)]
inferIOTypes opts valueanalysis astore flatprog = do
  rvmap <- loadAnalysisWithImports astore valueanalysis opts flatprog
  return $ map (inOutATypeFunc rvmap) (progFuncs flatprog)

--- Infer the in/out types of all functions in a program and return them
--- together with the number of all and public non-trivial in/out types.
printIOTypesIfDemanded :: TermDomain a => Options -> (QName -> Bool)
                       -> [(QName, InOutType a)] -> IO ()
printIOTypesIfDemanded opts isvisible iotypes
  | optVerb opts > 2
  = do printInfoLine "INPUT/OUTPUT TYPES OF ALL OPERATIONS:"
       printIOTypes opts iotypes
  | not (optIOTypes opts)
  = return ()
  | optFormat opts /= FormatText
  = do printWhenStatus opts "INPUT/OUTPUT TYPES OF ALL PUBLIC OPERATIONS:"
       printIOTypes opts 
         (filter (\(qf,_) -> not (optPublic opts) || isvisible qf) iotypes)
  | otherwise
  = do let ntiotypes    = filter (not . isAnyIOType . snd) iotypes
           pubntiotypes = filter (isvisible . fst) ntiotypes
       printInfoLine $ "NON-TRIVIAL INPUT/OUTPUT TYPES OF " ++
          (if optPublic opts then "PUBLIC" else "ALL") ++ " OPERATIONS:"
       printIOTypes opts (if optPublic opts then pubntiotypes else ntiotypes)

printIOTypes :: TermDomain a => Options -> [(QName, InOutType a)] -> IO ()
printIOTypes opts iotypes
  | optFormat opts == FormatJSON
  = putStrLn $ ppJSON $ JArray $ map ioType2JSON (sortFunResults iotypes)
  | optFormat opts == FormatXML
  = putStrLn $ showXmlDoc $ xml "results" $
      map ioType2XML (sortFunResults iotypes)
  | otherwise
  = printInfoLine $ unlines $ showFunResults showIOT (sortFunResults iotypes)
 where
  ioType2JSON ((mn,fn),fct) =
    JObject [("module", JString mn),
             ("name"  , JString fn),
             ("result", JString (show fct))]
             
  ioType2XML ((mn,fn),fct) =
    xml "operation" [xml "module" [xtxt mn],
                     xml "name"   [xtxt fn],
                     xml "result" [xtxt (show fct)]]

-- Shows the call and in/out type of a given function defined in the module.
showFunctionInfo :: TermDomain a => Options -> String -> VerifyState a -> IO ()
showFunctionInfo opts mname vst = do
  let f = optFunction opts
      qf = (mname, f)
  fdecls <- currentFuncDecls vst
  if qf `notElem` map funcName fdecls
    then printInfoLine $ "Function '" ++ snd qf ++ "' not defined!"
    else do
      let iot   = maybe (trivialInOutType 0) id (Map.lookup qf (vstIOTypes vst))
          ctype = maybe (Just [anyType]) id (Map.lookup qf (vstCallTypes vst))
      printInfoLine $ "Function '" ++ f ++ "':"
      printInfoLine $ "In/out type: " ++ showIOT iot
      printInfoLine $ "Call type  : " ++ prettyFunCallAType ctype
      maybe (return ())
            (\nfc -> printInfoLine $ showConditions fdecls [(qf,nfc)])
            (lookup qf (vstFunConds vst))

-- Try to verify a module, possibly in several iterations.
-- The second argument is the number of already performed iterations,
-- the first component of the result is the total number of iterations.
tryVerifyProg :: TermDomain a => Options -> Int -> VerifyState a -> String
              -> Map.Map QName [FuncDecl] -> [FuncDecl]
              -> IO (Int,VerifyState a)
tryVerifyProg opts numits vstate mname funusage fdecls = do
  st <- execStateT (mapM_ verifyFunc fdecls) vstate
  -- remove NewFailed information about unchanged operations
  -- (usually, these are failed operations with changed conditions)
  let newfailures = filter (\(qf,ct) -> maybe True (\fct -> ct /= fct)
                                          (Map.lookup qf (vstCallTypes st)))
                           (vstNewFailed st)
  unless (null newfailures || optVerb opts < 2) $ printFailures st
  unless (null newfailures) $ printWhenStatus opts $ unlines $
    "Operations with refined call types (used in future analyses):" :
    showFunResults prettyFunCallAType (reverse newfailures)
  let newcts = Map.union (Map.fromList newfailures) (vstCallTypes st)
  enforceNormalForm opts "NEWCALLTYPES" newcts
  let (failconds,refineconds) =
         partition (\(qf,_) -> qf `elem` (map fst (vstFunConds st)))
                   (vstNewFunConds st)
      newfailconds = filter (\(qf,_) -> (qf,nfcFalse) `notElem` vstFunConds st)
                            failconds
      -- Condition for next iteration: set already existing conditions to
      -- `False` to avoid infinite refinements
      nextfunconds = (unionBy (\x y -> fst x == fst y)
                              (map (\(qf,_) -> (qf, nfcFalse)) newfailconds)
                              (vstFunConds st)) ++ refineconds
      newrefineconds = newfailconds ++ refineconds
  --fdecls <- currentFuncDecls st
  unless (null newrefineconds) $ printWhenStatus opts $
    "Operations with refined call conditions (used in future analyses):\n" ++
    showConditions fdecls newrefineconds
  let st' = st { vstCallTypes = newcts, vstNewFailed = []
               , vstFunConds = nextfunconds, vstNewFunConds = [] }
  if null newfailures && null newrefineconds
    then do printFailures st'
            -- remove always failing conditions (since the call types are
            -- always failing for such functions):
            let st'' = st' { vstFunConds =
                               filter ((/= nfcFalse) . snd) nextfunconds }
            return (numits + 1, st'')
    else do
      let -- functions with refined call types:
          rfuns = map fst (filter (not . isFailACallType . snd) newfailures)
          newfdecls =
            foldr unionFDecls
              (filter (\fd -> funcName fd `elem` rfuns) fdecls)
              (map (\qf -> maybe [] id (Map.lookup qf funusage))
                   (union (map fst newfailures) (map fst newrefineconds)))
      printWhenStatus opts $ "Repeat verification with new call types..." ++
        "(" ++ show (length newfdecls) ++ " functions)"
      --printInfoLine $ unlines $
      --  showFunResults prettyFunCallAType (sortFunResults $ vstCallTypes st')
      tryVerifyProg opts (numits + 1) st' mname funusage newfdecls
 where
  failLine = take 78 (repeat '!')
  failComment = failLine ++ "\nPROGRAM CONTAINS POSSIBLY FAILING "

  printFailures st = whenStatus opts $ do
    unless (null (vstFailedFuncs st)) $
      printInfoLine $ failComment ++ "FUNCTION CALLS:\n" ++
         unlines (map (\ (qf,_,e) -> "Function '" ++ snd qf ++
                                     "': call '" ++ showExp e ++ "'")
                      (reverse (vstFailedFuncs st)) ++ [failLine])
    unless (null (vstPartialBranches st)) $
      printInfoLine $ failComment ++ "FUNCTIONS:\n" ++
         unlines
           (map (\ (qf,_,e,cs) -> showIncompleteBranch qf e cs)
                (reverse (vstPartialBranches st)) ++ [failLine])

--- Prints a message about the result of the module verification.
printVerifyResult :: TermDomain a => Options -> VerifyState a -> String
                  -> (QName -> Bool) -> IO ()
printVerifyResult opts vst mname isvisible = do
  fdecls <- currentFuncDecls vst
  printVerifyResults opts mname isvisible fdecls
                     (Map.toList (vstCallTypes vst)) (vstFunConds vst)

------------------------------------------------------------------------------
-- Print the verification results in the required format.
printVerifyResults :: TermDomain a => Options -> String -> (QName -> Bool)
                   -> [FuncDecl] -> [(QName,ACallType a)]
                   -> [(QName,NonFailCond)] -> IO ()
printVerifyResults opts mname isvisible fdecls ctypes nfconds
  | optVerb opts == 0
  = return ()
  | optFormat opts == FormatJSON
  = putStrLn $ ppJSON $ JArray $
      map callAType2JSON (sortFunResults (filter (showFun . fst) ctypes))
  | optFormat opts == FormatXML
  = putStrLn $ showXmlDoc $ xml "results" $
      map callAType2XML (sortFunResults (filter (showFun . fst) ctypes))
  | otherwise
  = do
    putStr $ "MODULE '" ++ mname ++ "' VERIFIED"
    let calltypes = filter (\(qf,ct) -> not (isTotalACallType ct) && showFun qf)
                           ctypes
        funconds = filter (showFun . fst) nfconds
    if null calltypes
      then putStrLn "\n"
      else putStrLn $ unlines $ " W.R.T. NON-TRIVIAL ABSTRACT CALL TYPES:"
             : showFunResults prettyFunCallAType
                 (sortFunResults (filter ((`notElem` (map fst funconds)) . fst)
                                         calltypes))
    unless (null funconds) $
      putStrLn $ "NON-FAIL CONDITIONS FOR OTHERWISE FAILING FUNCTIONS:\n" ++
                 showConditions fdecls (sortFunResults funconds)
 where
  showFun qf = not (optPublic opts) || isvisible qf

  callAType2JSON (qf@(mn,fn),fct) =
    JObject [("module", JString mn),
             ("name"  , JString fn),
             ("result", JString (showCallATypeOrNonFailCond qf fct))]
             
  callAType2XML (qf@(mn,fn),fct) =
    xml "operation" [xml "module" [xtxt mn],
                     xml "name"   [xtxt fn],
                     xml "result" [xtxt (showCallATypeOrNonFailCond qf fct)]]
             
  showCallATypeOrNonFailCond qf ct =
    maybe ("0:" ++ show ct)
          (\nfc -> "1:" ++ show (snd (genNonFailFunction fdecls (qf,nfc))))
          (lookup qf nfconds)

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

------------------------------------------------------------------------------
-- The state of the transformation process contains
-- * the list of all function declarations of the current module
-- * the current function to be analyzed (name, arity, rule arguments)
-- * a list of all constructors grouped by types
-- * a fresh variable index
-- * a list of all variables and their bound expressions
-- * a list of all variables and their input/output types
-- * the call condition of the current branch expressed as a (Boolean) FlatCurry
--   expression which contains a "hole" to extend it with further conjunctions
-- * the call types of all imported operations
-- * the call types of operations of the current module
-- * the input/output types of all operations
-- * the list of failed function calls
-- * the list of incomplete case expressions
-- * the list of functions marked as failing in this iteration
-- * some statistics
-- * the tool options
-- * a Boolean flag which is `True` if some error occurred
-- It is parameterized over the abstract term domain.
data VerifyState a = VerifyState
  { vstModules     :: IORef ProgInfo    -- all infos about loaded modules
  , vstCurrModule  :: String            -- name of the current module
  , vstCurrFunc    :: (QName,Int,[Int]) -- name/arity/args of current function
  , vstConsInfos   :: [(QName,ConsInfo)]-- infos about all constructors
  , vstFreshVar    :: Int               -- fresh variable index in a rule
  , vstVarExp      :: [(Int,TypeExpr,Expr)] -- map variable to its type and
                                            -- subexpression
  , vstVarTypes    :: VarTypesMap a     -- map variable to its abstract types
  , vstCondition   :: Expr -> Expr      -- current branch condition (with hole)
  , vstImpCallTypes:: Map.Map QName (ACallType a) -- call types of imports
  , vstCallTypes   :: Map.Map QName (ACallType a) -- call types of module
  , vstIOTypes     :: Map.Map QName (InOutType a) -- in/out type for all funcs
  , vstFailedFuncs :: [(QName,Int,Expr)]   -- functions with illegal calls
  , vstPartialBranches :: [(QName,Int,Expr,[QName])] -- incomplete branches
  , vstNewFailed   :: [(QName,ACallType a)] -- new failed function call types
  , vstImpFunConds :: Map.Map QName NonFailCond -- call conditions of imports
  , vstFunConds    :: [(QName,NonFailCond)] -- call conditions of functions
  , vstNewFunConds :: [(QName,NonFailCond)] -- functions with new call conds
  , vstStats       :: (Int,Int,Int)         -- number of: non-trivial calls /
                                            -- incomplete cases /
                                            -- SMT-checked non-trivial calls
  , vstToolOpts    :: Options
  , vstError       :: Bool
  }

--- Initializes the verification state.
initVerifyState :: TermDomain a => IORef ProgInfo -> Prog -> [(QName,ConsInfo)]
                -> Map.Map QName (ACallType a) -> Map.Map QName NonFailCond
                -> Map.Map QName (ACallType a)
                -> Map.Map QName (InOutType a)
                -> [(QName,NonFailCond)] -> Options
                -> IO (VerifyState a)
initVerifyState pistore flatprog consinfos impacalltypes impfconds acalltypes
                iotypes nfconds opts = do
  unless (null nonfailconds) $ printWhenDetails opts $
    "INITIAL NON-FAIL CONDITIONS:\n" ++
    showConditions (progFuncs flatprog) nonfailconds
  return $ VerifyState pistore (progName flatprog) (("",""),0,[]) consinfos 0
                       [] [] id impacalltypes nfacalltypes iotypes [] [] []
                       impfconds nonfailconds [] (0,0,0) opts False
 where
  nonfailconds = unionBy (\x y -> fst x == fst y) nfconds
                         (nonFailCondsOfModule flatprog)

  -- set the call types of operations with non-fail conditions to "failed"
  nfacalltypes = Map.insertList
                   (map (\(qf,_) -> (qf, failACallType)) nonfailconds)
                   acalltypes

-- The type of the state monad contains the verification state.
type VerifyStateM atype a = StateT (VerifyState atype) IO a

-- Sets the name and arity of the current function in the state.
setToolError :: TermDomain a => VerifyStateM a ()
setToolError = do
  st <- get
  put $ st { vstError = True }

-- Gets the function declarations of the current module.
currentFuncDecls :: TermDomain a => VerifyState a -> IO [FuncDecl]
currentFuncDecls st = do
   prog <- getFlatProgFor (vstModules st) (vstCurrModule st)
   return $ progFuncs prog

-- Sets the name and arity of the current function in the state.
setCurrentFunc :: TermDomain a => QName -> Int -> [Int] -> VerifyStateM a ()
setCurrentFunc qf ar vs = do
  st <- get
  put $ st { vstCurrFunc = (qf,ar,vs) }

-- Gets the name of the current function in the state.
getCurrentFuncName :: TermDomain a => VerifyStateM a QName
getCurrentFuncName = do
  st <- get
  return $ let (qf,_,_) = vstCurrFunc st in qf

-- Gets information about all constructors.
getConsInfos :: TermDomain a => VerifyStateM a [(QName,ConsInfo)]
getConsInfos = get >>= return . vstConsInfos

-- Sets the fresh variable index in the state.
setFreshVarIndex :: TermDomain a => Int -> VerifyStateM a ()
setFreshVarIndex fvi = do
  st <- get
  put $ st { vstFreshVar = fvi }

-- Gets a new fresh variable index.
newFreshVarIndex :: TermDomain a => VerifyStateM a Int
newFreshVarIndex = do
  v <- fmap vstFreshVar get
  setFreshVarIndex (v + 1)
  return v

-- Adds a new (more restricted) inferred call type for a function
-- which will be used in the next iteration. If there is already
-- a more restricted call type, they will be joined.
addCallTypeRestriction :: TermDomain a => QName -> ACallType a -> VerifyStateM a ()
addCallTypeRestriction qf ctype = do
  st <- get
  maybe (put $ st { vstNewFailed = (qf,ctype) : (vstNewFailed st) } )
        (\ct -> do
           let newct = joinACallType ct ctype
           put $ st { vstNewFailed = unionBy (\x y -> fst x == fst y)
                                             [(qf,newct)] (vstNewFailed st) })
        (lookup qf (vstNewFailed st))

-- Adds a new condition (provided as the second argument as a FlatCurry
-- expression) to the call condition for the current function (first argument)
-- so that it will be used in the next iteration.
-- If there is already a new refined call condition, they will be combined.
-- If the condition is `True`, i.e., the call condition cannot be refined,
-- then the current branch condition will be negated and added
-- so that the current branch is not reachable.
-- Furthermore, the call type of the current function is set to
-- always failing.
addConditionRestriction :: TermDomain a => QName -> Expr -> VerifyStateM a ()
addConditionRestriction qf cond = do
  st <- get
  when (optSMT (vstToolOpts st)) $ do
    let (_,_,vs) = vstCurrFunc st
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
setNewFunCondition :: TermDomain a => QName -> NonFailCond -> VerifyStateM a ()
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
    map (\ct -> if all isAnyType (argTypesOfCons ct (arityOfCons consinfos ct) at)
                  then transTester consinfos ct (Var v)
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
addFailedFunc :: TermDomain a => Expr -> Maybe [(Int,a)] -> Expr -> VerifyStateM a ()
addFailedFunc exp mbvts cond = do
  st <- get
  let (qf,ar,args) = vstCurrFunc st
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
addMissingCase :: TermDomain a => Expr -> [QName] -> VerifyStateM a ()
addMissingCase exp qcs = do
  st <- get
  let (qf,ar,_) = vstCurrFunc st
  put $
    st { vstPartialBranches = union [(qf,ar,exp,qcs)] (vstPartialBranches st) }
  addCallTypeRestriction qf failACallType

-- Sets the types and expressions for variables.
getVarExps :: TermDomain a => VerifyStateM a [(Int,TypeExpr,Expr)]
getVarExps = fmap vstVarExp get

-- Sets the types and expressions for variables.
setVarExps :: TermDomain a => [(Int,TypeExpr,Expr)] -> VerifyStateM a ()
setVarExps varexps = do
  st <- get
  put $ st { vstVarExp = varexps }

-- Sets the FlatCurry type of a given variable.
-- This is used to specialize numeric types during verification
-- of predefined numeric operations.
setVarExpTypeOf :: TermDomain a => Int -> TypeExpr -> VerifyStateM a ()
setVarExpTypeOf var te = do
  st <- get
  put $ st { vstVarExp = map (\(v,t,e) -> if v==var then (v,te,e) else (v,t,e))
                             (vstVarExp st) }

-- Adds types and expressions for new variables.
addVarExps :: TermDomain a => [(Int,TypeExpr,Expr)] -> VerifyStateM a ()
addVarExps varexps = do
  st <- get
  put $ st { vstVarExp = vstVarExp st ++ varexps }

-- Get all currently stored variable types.
getVarTypes :: TermDomain a => VerifyStateM a (VarTypesMap a)
getVarTypes = fmap vstVarTypes get

-- Gets the currently stored types for a given variable.
getVarTypeOf :: TermDomain a => Int -> VerifyStateM a (VarTypes a)
getVarTypeOf v = do
  st <- get
  return $ maybe [] id (lookup v (vstVarTypes st))

-- Sets all variable types.
setVarTypes :: TermDomain a => VarTypesMap a -> VerifyStateM a ()
setVarTypes vartypes = do
  st <- get
  put $ st { vstVarTypes = vartypes }

-- Adds a new variable type to the current set of variable types.
-- It could be an alternative type for an already existent variable or
-- a type for a new variable.
addVarType :: TermDomain a => Int -> VarTypes a -> VerifyStateM a ()
addVarType v vts = do
  st <- get
  put $ st { vstVarTypes = addVarType2Map v vts (vstVarTypes st) }

-- Adding multiple variable types:
addVarTypes :: TermDomain a => VarTypesMap a -> VerifyStateM a ()
addVarTypes vtsmap = do
  st <- get
  put $ st { vstVarTypes = concVarTypesMap (vstVarTypes st) vtsmap }

-- Adds a new variable `Any` type to the current set of variable types.
addVarAnyType :: TermDomain a => Int -> VerifyStateM a ()
addVarAnyType v = addVarType v (ioVarType anyType)

-- Removes an `Any` type for a given variable from the current
-- set of variable types.
-- Used to remove the initial `Any` types in let bindings.
removeVarAnyType :: TermDomain a => Int -> VerifyStateM a ()
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
getCondition :: TermDomain a => VerifyStateM a (Expr -> Expr)
getCondition = fmap vstCondition get

-- Gets the expanded current branch condition.
getExpandedCondition :: TermDomain a => VerifyStateM a Expr
getExpandedCondition = do
  st <- get
  return $ expandExpr (vstVarExp st) (vstCondition st fcTrue)

-- Gets the expanded current branch condition where a conjunct has been added.
getExpandedConditionWithConj :: TermDomain a => Expr -> VerifyStateM a Expr
getExpandedConditionWithConj conj = do
  st <- get
  return $ expandExpr (vstVarExp st) (vstCondition st conj)

-- Sets the current branch condition in internal representation.
setCondition :: TermDomain a => (Expr -> Expr) -> VerifyStateM a ()
setCondition expf = do
  st <- get
  put $ st { vstCondition = expf }

-- Sets the initial condition for a function call.
setCallCondition :: TermDomain a => Expr -> VerifyStateM a ()
setCallCondition exp = do
  st <- get
  put $ st { vstCondition = fcAnd exp }

-- Adds a conjunct to the current condition.
addConjunct :: TermDomain a => Expr -> VerifyStateM a ()
addConjunct exp = do
  st <- get
  put $ st { vstCondition = \c -> (vstCondition st) (fcAnd exp c) }

-- Adds to the current condition an equality between a variable and
-- a constructor applied to a list of fresh pattern variables.
-- This condition is expressed as a case expression where
-- the hole of the current condition is in the branch for the given
-- constructor and all alternative branches return a `False` value.
addSingleCase :: TermDomain a => Int -> QName -> [Int] -> VerifyStateM a ()
addSingleCase casevar qc branchvars = do
  st <- get
  let siblings    = siblingsOfCons (vstConsInfos st) qc
      catchbranch = if null siblings then []
                                     else [Branch (Pattern anonCons []) fcFalse]
  put $ st { vstCondition =
               \c -> (vstCondition st)
                       (Case Rigid (Var casevar)
                          ([Branch (Pattern qc branchvars) c] ++ catchbranch)) }

-- Adds an equality between a variable and an expression as a conjunct
-- to the current condition.
addEquVarCondition :: TermDomain a => Int -> Expr -> VerifyStateM a ()
addEquVarCondition var exp = do
  let conj = if exp == fcTrue
               then Var var
               else if exp == fcFalse
                      then fcNot (Var var)
                      else Comb FuncCall (pre "==") [Var var, exp]
                           -- note: the Eq class dictioniary is missing...
  addConjunct conj

-- Gets the possible non-fail condition of a given operation.
getNonFailConditionOf :: TermDomain a => QName -> VerifyStateM a (Maybe NonFailCond)
getNonFailConditionOf qf = do
  st <- get
  return $ maybe (Map.lookup qf (vstImpFunConds st))
                 Just
                 (lookup qf (vstFunConds st))

-- Gets the abstract call type of a given operation.
-- The trivial abstract call type is returned for encapsulated search operations.
getCallType :: TermDomain a => QName -> Int -> VerifyStateM a (ACallType a)
getCallType qf ar
  | isEncSearchOp qf || isSetFunOp qf
  = return trivialACallType
  | otherwise
  = do
  st <- get
  return $
    if qf == pre "error" && optError (vstToolOpts st)
      then failACallType
      else maybe (maybe (trace ("Warning: call type of operation " ++
                                show qf ++ " not found!") trivialACallType)
                        id
                        (Map.lookup qf (vstImpCallTypes st)))
                 id
                 (Map.lookup qf (vstCallTypes st))
 where
  trivialACallType = Just $ take ar (repeat anyType)

-- Gets the in/out type for an operation of a given arity.
-- If the operation is not found, returns a general type.
-- The trivial in/out type is returned for encapsulated search operations.
getFuncType :: TermDomain a => QName -> Int -> VerifyStateM a (InOutType a)
getFuncType qf ar
  | isEncSearchOp qf || isSetFunOp qf
  = return $ trivialInOutType ar
  | otherwise
  = do st <- get
       maybe (do lift $ printInfoLine $
                   "WARNING: in/out type of '" ++ show qf ++ "' not found!"
                 return $ trivialInOutType ar)
             return
             (Map.lookup qf (vstIOTypes st))

-- Increment number of checks of non-trivial function calls.
incrNonTrivialCall :: TermDomain a => VerifyStateM a ()
incrNonTrivialCall = do
  st <- get
  put $ st { vstStats = (\ (f,c,s) -> (f+1,c,s)) (vstStats st) }

-- Increment number of checks of incomplete case expressions.
incrIncompleteCases :: TermDomain a => VerifyStateM a ()
incrIncompleteCases = do
  st <- get
  put $ st { vstStats = (\ (f,c,s) -> (f,c+1,s)) (vstStats st) }

-- Increment number of checks of unsatisfiability with SMT.
incrUnsatSMT :: TermDomain a => VerifyStateM a ()
incrUnsatSMT = do
  st <- get
  put $ st { vstStats = (\ (f,c,s) -> (f,c,s+1)) (vstStats st) }

-- Gets the tool options from the current state.
getToolOptions :: TermDomain a => VerifyStateM a Options
getToolOptions = get >>= return . vstToolOpts

--- Prints a string with `printInfoString` if the verbosity is at least as
--- the given one.
printIfVerb :: TermDomain a => Int -> String -> VerifyStateM a ()
printIfVerb v s = do
  opts <- getToolOptions
  when (optVerb opts >= v) $ lift $ printInfoString s

------------------------------------------------------------------------------

-- Verify a FlatCurry function declaration.
verifyFunc :: TermDomain a => FuncDecl -> VerifyStateM a ()
verifyFunc (Func qf ar _ ftype rule) = case rule of
  Rule vs exp -> unless noVerify $ do
                   setCurrentFunc qf ar vs
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

verifyFuncRule :: TermDomain a => [Int] -> TypeExpr -> Expr -> VerifyStateM a ()
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
showVarExpTypes :: TermDomain a => VerifyStateM a ()
showVarExpTypes = do
  qf <- getCurrentFuncName
  opts <- getToolOptions
  when (optVerb opts > 3) $ do
    st <- get
    lift $ printInfoString $
      "Current set of variables in function " ++ snd qf ++
      ":\nVariable bindings:\n" ++
      unlines (map (\ (v,te,e) -> showBindExp v e ++
                     if te == unknownType then "" else " :: " ++ showTypeExp te)
                   (vstVarExp st))
    vartypes <- getVarTypes
    lift $ printInfoString $ "Variable types\n" ++ showVarTypes vartypes
    cond <- getExpandedCondition
    lift $ printInfoLine $ "Current condition: " ++ showSimpExp cond

-- Verify an expression (if the first argument is `True`) and,
-- if the expression is not a variable, create a fresh
-- variable and a binding for this variable.
-- The variable which identifies the expression is returned.
verifyExpr :: TermDomain a => Bool -> Expr -> VerifyStateM a Int
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
verifyVarExpr :: TermDomain a => Int -> Expr -> VerifyStateM a (VarTypesMap a)
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
verifyFuncCall :: TermDomain a => Expr -> QName -> [Int] -> VerifyStateM a ()
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
                      -> ACallType a -> Maybe NonFailCond -> VerifyStateM a ()
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
checkPredefinedOp :: TermDomain a => Expr -> VerifyStateM a (VarTypesMap a)
                  -> VerifyStateM a (VarTypesMap a)
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
verifyMissingBranches :: TermDomain a => Expr -> Int -> [BranchExpr] -> VerifyStateM a ()
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
      siblings = siblingsOfCons consinfos qc
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
    let iscons = transTester consinfos cs (Var casevar)
    bcond <- getExpandedCondition
    unsat <- isUnsatisfiable (fcAnd iscons bcond)
    return $ if unsat then [] else [cs]

-- Gets the state information which might be changed during branch verification.
getBranchState :: TermDomain a => VerifyStateM a ([(Int,TypeExpr,Expr)], VarTypesMap a, Expr -> Expr)
getBranchState = do
  ves  <- getVarExps
  vts  <- getVarTypes
  cond <- getCondition
  return (ves,vts,cond)

-- Gets the state information which might be changed during branch verification.
restoreBranchState :: TermDomain a =>
  ([(Int,TypeExpr,Expr)], VarTypesMap a, Expr -> Expr) -> VerifyStateM a ()
restoreBranchState (ves,vts,cond) = do
  setVarExps ves
  setVarTypes vts
  setCondition cond

-- Verify a branch where the first argument is the case argument variable
-- and the second argument is the variable identifying the case expression.
verifyBranch :: TermDomain a => Int -> Int -> BranchExpr
             -> VerifyStateM a (VarTypesMap a)
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
funcDecls2Usage :: String -> [FuncDecl] -> Map.Map QName [FuncDecl]
funcDecls2Usage mname fdecls = addFDecls (Map.empty) fdecls
 where
  addFDecls m []       = m
  addFDecls m (fd:fds) =
    let rhsfuns = filter (\f -> fst f == mname) (usedFuncsInFunc fd)
    in Map.insertListWith unionFDecls (map (\qf -> (qf,[fd])) rhsfuns)
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
isUnsatisfiable :: TermDomain a => Expr -> VerifyStateM a Bool
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
      unless (all (`elem` map fst vtypes) allvs) $ lift $ printInfoLine $
        "WARNING in operation '" ++ snd fname ++
        "': missing variables in unsatisfiability check!"
      consinfos <- getConsInfos
      answer <- lift $ checkUnsatisfiabilityWithSMT (vstToolOpts st)
                         fname question (vstModules st) consinfos vtypes bexp
      maybe (setToolError >> return False) return answer
    else return False

------------------------------------------------------------------------------
