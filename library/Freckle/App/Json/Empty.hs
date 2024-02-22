-- | Aeson represents
module Freckle.App.Json.Empty
  ( Empty (..)
  ) where

import Freckle.App.Prelude

import Autodocodec (Autodocodec (..), HasCodec (..), HasObjectCodec (..), object)
import Autodocodec.OpenAPI ()
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema (..))
import Test.QuickCheck (Arbitrary (..))

-- | A unit value encoded as an empty JSON object
--
-- Useful as the response body of a POST request when the server doesn't
-- need to return anything.
--
-- (One would expect to be able to use () for this, but Aeson encodes unit
-- as an empty list, not as an object.)
data Empty = Empty
  deriving (ToJSON, FromJSON, ToSchema) via (Autodocodec Empty)

instance Arbitrary Empty where
  arbitrary = pure Empty

instance HasCodec Empty where
  codec = object "Empty" objectCodec

instance HasObjectCodec Empty where
  objectCodec = pure Empty

instance Semigroup Empty where
  _ <> _ = Empty

instance Monoid Empty where
  mempty = Empty
