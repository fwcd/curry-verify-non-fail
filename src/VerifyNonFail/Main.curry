-------------------------------------------------------------------------
--- A tool to verify Curry programs w.r.t. failing computations.
--- Thus, a program successfully verified by this tool should never
--- fail at run-time (apart from explicit error) provided that
--- the call types are satisfied when invoking a function.
---
--- @author Michael Hanus
--- @version May 2025
-------------------------------------------------------------------------

module VerifyNonFail.Main ( main ) where

import Control.Monad               ( unless, when )
import System.Environment          ( getArgs )
import Text.Pretty                 ( pPrint )

-- Imports from dependencies:
import Analysis.Types             ( Analysis, analysisName )
import Analysis.Values
import System.Directory           ( createDirectoryIfMissing, doesFileExist
                                  , removeDirectory )
import System.FilePath            ( (</>) )
import System.Path                ( fileInPath )
import System.Process             ( exitWith )
import Verification.Run           ( runUntypedVerification )
import Verification.Options       ( VOptions (..), defaultVOptions )
import Verification.State         ( ppVState )
import Verification.Types         ( UVerification, Verification (..), emptyVerification )
import XML

-- Imports from package modules:
import Legacy.Run                 ( runLegacy )
import VerifyNonFail.Files        ( deleteVerifyCacheDirectory )
import VerifyNonFail.NonFailInfo  ( ppNonFailInfo )
import VerifyNonFail.Options
import VerifyNonFail.ProgInfo
import VerifyNonFail.Statistics
import VerifyNonFail.Verification ( nonFailVerifier )
import VerifyNonFail.WithSMT

------------------------------------------------------------------------------

banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "Curry Non-Failure Verifier (Verification Framework Alpha)"
  bannerLine = take (length bannerText) (repeat '=')

------------------------------------------------------------------------------

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
      let vopts = defaultVOptions
                    { voModules = ms
                    -- TODO: Logging etc.
                    }
      printWhenStatus opts banner
      if optLegacy opts
        then runLegacy opts ms
        else do
          result <- runUntypedVerification (nonFailVerifier opts) vopts
          case result of
            Left e  -> putStrLn ("Verification failed: " ++ e) >> exitWith 1
            Right s -> do
              putStrLn . pPrint $ ppVState ppNonFailInfo s

------------------------------------------------------------------------------
