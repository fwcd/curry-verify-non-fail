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
import Legacy.Run                 ( runLegacy )
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
  bannerText = "Curry Non-Failure Verifier (Verification Framework Alpha)"
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
      printWhenStatus opts banner
      if optLegacy opts
        then runLegacy opts ms
        else putStrLn "TODO: The framework implementation"

------------------------------------------------------------------------------
