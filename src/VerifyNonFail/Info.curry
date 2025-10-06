module VerifyNonFail.Info
  ( VerifyInfo (..), emptyVerifyInfo, mapVerifyInfoDomain, ppVerifyInfo
  ) where

import Prelude hiding ( empty )

import Analysis.TermDomain          ( TermDomain(..) )
import Data.Maybe                   ( mapMaybe )
import Text.Pretty                  ( Doc, (<+>), (<>), text, vcat, align, hsep, fill, empty, comma )
import VerifyNonFail.CallTypes      ( ACallType, mapATypeDomain, prettyCT, prettyFunCallAType )
import VerifyNonFail.Conditions     ( NonFailCond )
import VerifyNonFail.IOTypes        ( InOutType, mapIOTDomain, showIOT )
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

instance Monoid (VerifyInfo a) where
  mempty = emptyVerifyInfo
  mappend = combineVerifyInfo

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

--- Combines two VerifyInfos.
combineVerifyInfo :: VerifyInfo a -> VerifyInfo a -> VerifyInfo a
combineVerifyInfo v1 v2 = VerifyInfo
  { viNonFailCond = viNonFailCond v1 <|> viNonFailCond v2
  , viCallType    = viCallType    v1 <|> viCallType    v2
  , viIOType      = viIOType      v1 <|> viIOType      v2
  }

--- Pretty-prints the given non-failure info in human-readable format.
ppVerifyInfo :: TermDomain a => VerifyInfo a -> Doc
ppVerifyInfo vi = align . csep . mapMaybe (\(label, v) -> (text label <+>) . text <$> v) $
  [ ("non-failure condition:", show <$> viNonFailCond vi)
  , ("call type:",  prettyFunCallAType <$> viCallType vi)
  , ("i/o type:", showIOT <$> viIOType vi)
  ]
  where csep = foldr1 (\x y -> x <> comma <+> y)
