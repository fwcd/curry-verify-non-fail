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
import Control.Monad.Trans.Class   ( MonadTrans (..) )
import Control.Monad.Trans.State   ( execStateT, modify )
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
import Verification.Log           ( VLevel (..), printLog, withVLevel )
import Verification.Run           ( runUntypedVerification )
import Verification.Options       ( VOptions (..), buildVOptions )
import Verification.State         ( ppVState )
import Verification.Types         ( UVerification, Verification (..), emptyVerification )
import XML

-- Imports from package modules:
import Legacy.Run                 ( runLegacy )
import VerifyNonFail.Files        ( deleteVerifyCacheDirectory )
import VerifyNonFail.Info         ( ppVerifyInfo )
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

  opts <- flip execStateT opts0 $ do
    -- set analysis to top values if unspecified
    when (null (optDomainID opts0)) $ do
      modify $ \o -> o { optDomainID = analysisName resultValueAnalysisTop }
    
    -- check if Z3 can be found and disable SMT otherwise
    z3exists <- lift $ fileInPath "z3"
    let z3msg = "Option '--nosmt' activated since SMT solver Z3 not found in PATH!"
    unless (z3exists || not (optSMT opts0)) $ do
      lift $ printInfoLine z3msg
      modify $ \o -> o { optSMT = False }

  when (optDeleteCache opts0) $ deleteVerifyCacheDirectory opts0
  case progs of
    [] -> unless (optDeleteCache opts0) $ do
            printInfoLine "Module name missing!"
            printInfoLine "Try option '--help' for usage information."
            exitWith 1
    ms -> do
      let vlvl  = case optVerb opts of
                    v | v > 2     -> VLevelAll
                      | v > 1     -> VLevelDebug
                      | v > 0     -> VLevelInfo
                      | otherwise -> VLevelNone

      vopts <- buildVOptions "VerifyNonFail" $ \o -> o
        { voModules = ms
        , voLog     = withVLevel vlvl printLog
        }

      printWhenStatus opts banner
      if optLegacy opts
        then runLegacy opts ms
        else case nonFailVerifier opts of
          Left s  -> putStrLn ("Could not create verification: " ++ s) >> exitWith 1
          Right v -> do
            result <- runUntypedVerification v vopts
            case result of
              Left e  -> putStrLn ("Verification failed: " ++ e) >> exitWith 1
              Right s -> putStrLn . pPrint $ ppVState ppVerifyInfo s

------------------------------------------------------------------------------
