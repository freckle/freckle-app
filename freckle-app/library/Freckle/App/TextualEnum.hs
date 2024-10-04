{-# LANGUAGE UndecidableInstances #-}

-- | Typical instances for enumerated data types with textual representation
module Freckle.App.TextualEnum
  ( TextualEnum (..)
  , EnumValue (..)
  , EnumName (..)
  , enums
  , fromText
  , prop_roundTripEnumText
  ) where

import Freckle.App.Prelude

import Autodocodec
import Control.Lens hiding (elements)
import Control.Monad (mzero)
import Data.Aeson
import Data.Csv qualified as CSV
import Data.Dynamic (Typeable)
import Data.List.NonEmpty qualified as NE
import Data.OpenApi
import Data.Text.Encoding qualified as T
import Database.Persist.Sql
  ( PersistField (..)
  , PersistFieldSql (..)
  , SqlType (..)
  )
import Servant
import Test.QuickCheck (Arbitrary (..), elements)
import Web.PathPieces

class EnumValue a where
  -- | Convert a 'TextualEnum' to 'Text'
  toText :: a -> Text

class EnumName a where
  -- | Name of a 'TextualEnum', used for naming schemas
  enumName :: Proxy a -> Text

-- | Wrapper around enums
--
-- N.B. This should not be used for "enormous" enumerations. It's primary purpose
-- is to provide standard instances for discriminated-union-defined enums.
newtype TextualEnum a = TextualEnum {enumValue :: a}
  deriving newtype (Eq, Show, Ord, Generic)

-- | All values of a 'TextualEnum'
enums :: (Bounded a, Enum a) => NonEmpty (TextualEnum a)
enums = fmap TextualEnum $ minBound NE.:| drop 1 [minBound .. maxBound]

-- | Parse a 'TextualEnum' from 'Text'
fromText :: (EnumValue a, Bounded a, Enum a) => Text -> Maybe (TextualEnum a)
fromText txt = find ((== txt) . toText . enumValue) enums

instance EnumValue a => ToJSON (TextualEnum a) where
  toJSON = toJSON . toText . enumValue
  toEncoding = toEncoding . toText . enumValue

instance (EnumValue a, Bounded a, Enum a) => FromJSON (TextualEnum a) where
  parseJSON = withText "TextualEnum" $ maybe mzero pure . fromText

instance (Bounded a, Enum a) => Arbitrary (TextualEnum a) where
  arbitrary = elements $ toList enums

instance (EnumValue a, Bounded a, Enum a) => PathPiece (TextualEnum a) where
  toPathPiece = toPathPiece . toText . enumValue
  fromPathPiece = fromText

instance EnumValue a => CSV.ToField (TextualEnum a) where
  toField = CSV.toField . toText . enumValue

instance (EnumValue a, Bounded a, Enum a) => CSV.FromField (TextualEnum a) where
  parseField = maybe mzero pure . fromText . T.decodeUtf8

instance
  (Bounded a, Enum a, EnumValue a, Typeable a, EnumName a)
  => ToSchema (TextualEnum a)
  where
  declareNamedSchema = pure . NamedSchema (Just $ enumName $ Proxy @a) . enumOptions
   where
    enumOptions _ =
      mempty
        & type_ ?~ OpenApiString
        & enum_ ?~ (toJSON . toText . enumValue <$> toList (enums @a))
        & example ?~ (toJSON . toText . enumValue $ NE.head (enums @a))

instance (Bounded a, Enum a, EnumValue a) => ToParamSchema (TextualEnum a) where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & enum_ ?~ (toJSON . toText . enumValue <$> toList (enums @a))
      & example ?~ (toJSON . toText . enumValue $ NE.head (enums @a))

instance (Bounded a, Enum a, EnumValue a, Eq a) => HasCodec (TextualEnum a) where
  codec = stringConstCodec $ (id &&& (toText . enumValue)) <$> enums @a

instance (Bounded a, Enum a, EnumValue a) => PersistField (TextualEnum a) where
  toPersistValue = toPersistValue . toText . enumValue
  fromPersistValue =
    maybe (Left "Not member of enumeration") Right . fromText <=< fromPersistValue

instance (Bounded a, Enum a, EnumValue a) => PersistFieldSql (TextualEnum a) where
  sqlType _ = SqlString

instance (Bounded a, Enum a, EnumValue a) => FromHttpApiData (TextualEnum a) where
  parseUrlPiece =
    maybe (Left "Not member of enumeration") Right . fromText

instance EnumValue a => ToHttpApiData (TextualEnum a) where
  toUrlPiece = toText . enumValue

-- | Test that enum instances are coherent
prop_roundTripEnumText
  :: (Bounded a, Enum a, EnumValue a, Eq a) => TextualEnum a -> Bool
prop_roundTripEnumText a = fromText (toText $ enumValue a) == Just a
