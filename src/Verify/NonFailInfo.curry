module Verify.NonFailInfo
  ( NonFailInfo (..), ppNonFailInfo
  ) where

import Text.Pretty                 ( Doc, text )

--- The inferred call types/non-failure conditions.
data NonFailInfo = NonFailInfo -- TODO: Define this (e.g. call types etc.)
  deriving (Show, Eq)

--- Pretty-prints the given non-failure info.
ppNonFailInfo :: NonFailInfo -> Doc
ppNonFailInfo = text . show -- TODO: Better implementation
