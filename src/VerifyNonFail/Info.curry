module VerifyNonFail.Info
  ( VerifyInfo (..), emptyVerifyInfo, mapVerifyInfoDomain, ppVerifyInfo
  ) where

import Text.Pretty                  ( Doc, text )
import VerifyNonFail.CallTypes      ( ACallType, mapATypeDomain )
import VerifyNonFail.Conditions     ( NonFailCond )
import VerifyNonFail.IOTypes        ( InOutType, mapIOTDomain )
import VerifyNonFail.ProgInfo       ( ConsInfo )
import Verification.FlatCurry.Types ( QName )

--- Verification-related information about a function, optionally
--- including a non-failure condition, call type and I/O type.
data VerifyInfo a = VerifyInfo
  { viNonFailCond :: Maybe NonFailCond
  , viCallType    :: Maybe (ACallType a)
  , viIOType      :: Maybe (InOutType a)
  }
  deriving (Read, Show, Eq)

instance Functor VerifyInfo where
  fmap = mapVerifyInfoDomain

--- Creates an empty VerifyInfo.
emptyVerifyInfo :: VerifyInfo a
emptyVerifyInfo = VerifyInfo
  { viNonFailCond = Nothing
  , viCallType    = Nothing
  , viIOType      = Nothing
  }

--- Maps over the abstract term domain of a VerifyInfo.
mapVerifyInfoDomain :: (a -> b) -> VerifyInfo a -> VerifyInfo b
mapVerifyInfoDomain f vi = VerifyInfo
  { viNonFailCond = viNonFailCond vi
  , viCallType    = mapATypeDomain f <$> viCallType vi
  , viIOType      = mapIOTDomain f <$> viIOType vi
  }

--- Pretty-prints the given non-failure info.
ppVerifyInfo :: Show a => VerifyInfo a -> Doc
ppVerifyInfo = text . show -- TODO: Better implementation
