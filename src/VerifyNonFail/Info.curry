module VerifyNonFail.Info
  ( VerifyInfo (..), emptyVerifyInfo, mapVerifyInfoDomain, ppVerifyInfo
  ) where

import Prelude hiding ( empty )

import Analysis.TermDomain          ( TermDomain(..) )
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

--- Pretty-prints the given non-failure info in human-readable format.
ppVerifyInfo :: TermDomain a => VerifyInfo a -> Doc
ppVerifyInfo vi = align $ foldr1 (\x y -> x <> comma <+> y)
  [ text "nfc:" <+> text (maybe "none" show (viNonFailCond vi))
  , text "ct:"  <+> text (maybe "none" prettyFunCallAType (viCallType vi))
  , text "i/o:" <+> text (maybe "none" showIOT (viIOType vi))
  ]
