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
import Verification.Env           ( VUFuncEnv )
import Verification.Run           ( runUntypedVerification )
import Verification.Options       ( VOptions (..), defaultVOptions )
import Verification.Monad         ( VM, throwVM )
import Verification.State         ( ppVState )
import Verification.Types         ( UVerification, Verification (..), emptyVerification )
import Verification.Update        ( VFuncUpdate (..), VUFuncUpdate, VUProgUpdate, simpleVFuncUpdate, emptyVProgUpdate, emptyVFuncUpdate )
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
nonFailVerifier :: Options -> UVerification NonFailInfo
nonFailVerifier opts = emptyVerification
  { vInit   = initFuncInfo
  , vUpdate = updateFuncInfo opts
  }

initFuncInfo :: VUFuncEnv NonFailInfo -> VM (Maybe NonFailInfo)
initFuncInfo env = do
  return Nothing -- TODO

updateFuncInfo :: Options -> VUFuncEnv NonFailInfo -> VM (VUFuncUpdate NonFailInfo)
updateFuncInfo opts env = do
  return emptyVFuncUpdate -- TODO

------------------------------------------------------------------------------
