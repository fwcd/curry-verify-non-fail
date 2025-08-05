module VerifyNonFail.Info
  ( VerifyInfo (..), ppVerifyInfo
  ) where

import Text.Pretty                 ( Doc, text )

--- The inferred call types/non-failure conditions.
data VerifyInfo = VerifyInfo -- TODO: Define this (e.g. call types etc.)
  deriving (Show, Eq)

--- Pretty-prints the given non-failure info.
ppVerifyInfo :: VerifyInfo -> Doc
ppVerifyInfo = text . show -- TODO: Better implementation
