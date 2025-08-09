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
import qualified Data.Map as Map
import qualified Data.Set as Set
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
import Verification.Env           ( VUFuncEnv, VUProgEnv )
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
nonFailVerifier :: Options -> Either String (UVerification (VerifyInfo AnyDomain))
nonFailVerifier opts =
  if did == analysisName resultValueAnalysisTop
    then Right . invmap (TopDomain <$>) (fromTopDomain <$>) $ nonFailureVerifierWith resultValueAnalysisTop opts
    else if did == analysisName resultValueAnalysis2
      then Right . invmap (D2Domain <$>) (fromD2Domain <$>) $ nonFailureVerifierWith resultValueAnalysis2 opts
      else if did == analysisName resultValueAnalysis5
        then Right . invmap (D5Domain <$>) (fromD5Domain <$>) $ nonFailureVerifierWith resultValueAnalysis5 opts
        else Left $ "Unknown analysis domain ID: " ++ did
  where did = optDomainID opts

nonFailureVerifierWith :: TermDomain a => Analysis a -> Options -> UVerification (VerifyInfo a)
nonFailureVerifierWith valueanalysis opts = emptyVerification
  { vPreprocess = preprocessProg
  , vInit       = initFuncInfo
  , vUpdate     = updateFuncInfo valueanalysis opts
  }

preprocessProg :: TermDomain a => VUProgEnv (VerifyInfo a) -> VM VUProgUpdate
preprocessProg env = do
  return emptyVProgUpdate

initFuncInfo :: TermDomain a => VUFuncEnv (VerifyInfo a) -> VM (Maybe (VerifyInfo a))
initFuncInfo env = do
  return Nothing -- TODO

updateFuncInfo :: TermDomain a => Analysis a -> Options -> VUFuncEnv (VerifyInfo a) -> VM (VUFuncUpdate (VerifyInfo a))
updateFuncInfo valueanalysis opts env = do
  return emptyVFuncUpdate -- TODO

------------------------------------------------------------------------------
