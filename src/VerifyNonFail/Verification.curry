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
import Verification.Env           ( VUFuncEnv, VUProgEnv, currentProg )
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
  gs <- newIORef emptyGlobalState
  return emptyVerification
    { vPreprocess = preprocessProg gs
    , vInit       = initFuncInfo gs
    , vUpdate     = updateFuncInfo gs valueanalysis opts
    }

preprocessProg :: TermDomain a => IORef VerifyGlobalState -> VUProgEnv (VerifyInfo a) -> VM VUProgUpdate
preprocessProg gs env = do
  prog <- currentProg env

  -- Compute cons infos for module
  let modconsinfos = consInfoOfTypeDecls (progTypes prog)
  liftIO $ modifyIORef gs $ \s -> s { vgsConsInfos = M.union (M.fromList modconsinfos) (vgsConsInfos s) }

  return emptyVProgUpdate

initFuncInfo :: TermDomain a => IORef VerifyGlobalState -> VUFuncEnv (VerifyInfo a) -> VM (Maybe (VerifyInfo a))
initFuncInfo gs env = do
  -- consinfos <- TODO

  -- infer initial abstract call type:
  -- TODO

  return . Just $ emptyVerifyInfo

updateFuncInfo :: TermDomain a => IORef VerifyGlobalState -> Analysis a -> Options -> VUFuncEnv (VerifyInfo a) -> VM (VUFuncUpdate (VerifyInfo a))
updateFuncInfo gs valueanalysis opts env = do
  return emptyVFuncUpdate -- TODO

--- Global internal state that is held across the whole verification
--- lifecycle in an IORef. Mostly used for non-failure verification-specific
--- caching purposes.
data VerifyGlobalState = VerifyGlobalState
  { vgsConsInfos :: M.Map QName ConsInfo  -- infos about all constructors
  }

emptyGlobalState :: VerifyGlobalState
emptyGlobalState = VerifyGlobalState
  { vgsConsInfos = M.empty
  }

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
  , vstStats           :: (Int,Int,Int)              -- number of: non-trivial calls /
                                                     -- incomplete cases /
                                                     -- SMT-checked non-trivial calls
  , vstToolOpts        :: Options
  , vstError           :: Bool
  }

------------------------------------------------------------------------------
