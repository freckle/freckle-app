{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Stream, parse, and validate CSVs
--
-- A minor extension of [cassava](https://hackage.haskell.org/package/cassava).
-- Using `MonadValidate` and `Conduit`.
module Freckle.App.Csv
  ( csvWithValidationSink
  , csvWithParserAndValidationSink

    -- * Conduit Primitives
  , runCsvConduit
  , decodeCsv

    -- * Header Validation
  , ValidateHeader
  , validateHeader
  , hasHeader
  , defaultValidateOrderedHeader

    -- * Exceptions
  , CsvException (..)

    -- * Options
  , defaultOptions
  ) where

import Freckle.App.Prelude

import Conduit
import Control.Monad (foldM)
import Control.Monad.Validate
  ( MonadValidate (..)
  , Validate
  , ValidateT
  , refute
  , runValidate
  , runValidateT
  )
import Data.Aeson (KeyValue (..), ToJSON (..), object, pairs, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.Text as Conduit
import Data.Csv
  ( DefaultOrdered
  , FromNamedRecord (..)
  , Header
  , Name
  , NamedRecord
  , Parser
  , defaultDecodeOptions
  , defaultOptions
  , headerOrder
  )
import qualified Data.Csv.Incremental as CsvI
import Data.Functor.Bind (Bind)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (Proxy))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

-- | Treat CSV header line as 1
--
-- CSVs can break rows over lines, but we don't currently handle that.
headerLineNumber :: Int
headerLineNumber = 1

class ValidateHeader a where
  validateHeader
    :: (Bind m, Monad m) => proxy a -> Header -> ValidateT (NESeq String) m ()

hasHeader :: Monad m => Header -> Name -> ValidateT (NESeq String) m ()
hasHeader h name
  | name `V.elem` h = pure ()
  | otherwise = refute . pure $ BS8.unpack name

defaultValidateOrderedHeader
  :: forall a proxy m
   . (DefaultOrdered a, Monad m)
  => proxy a
  -> Header
  -> ValidateT (NESeq String) m ()
defaultValidateOrderedHeader _ h =
  traverse_ (h `hasHeader`) $ headerOrder (error "old school haskell" :: a)

-- | Stream parse a CSV
--
-- - Expects UTF-8
-- - Provides incremental validation
csvWithValidationSink
  :: forall a b err m
   . ( MonadThrow m
     , MonadUnliftIO m
     , PrimMonad m
     , ValidateHeader a
     , FromNamedRecord a
     )
  => ConduitT () ByteString (ResourceT m) ()
  -- ^ CSV as a byte stream
  -> (Vector a -> Validate (NonEmpty (CsvException err)) (Vector b))
  -- ^ Validation to apply to resulting rows
  -> m (Validate (NonEmpty (CsvException err)) (Vector b))
csvWithValidationSink =
  csvWithParserAndValidationSink (validateHeader (Proxy @a)) parseNamedRecord

-- | Stream parse a CSV with a custom parser
--
-- - Expects UTF-8
-- - Provides incremental validation
csvWithParserAndValidationSink
  :: forall a b err m
   . (MonadThrow m, MonadUnliftIO m, PrimMonad m)
  => (Header -> Validate (NESeq String) ())
  -> (NamedRecord -> Parser a)
  -- ^ Custom record parser
  -> ConduitT () ByteString (ResourceT m) ()
  -- ^ CSV as a byte stream
  -> (Vector a -> Validate (NonEmpty (CsvException err)) (Vector b))
  -- ^ Validation to apply to resulting rows
  -> m (Validate (NonEmpty (CsvException err)) (Vector b))
csvWithParserAndValidationSink headerValidator p source validation = do
  validatedCsv <-
    runCsvConduit $
      transPipe lift source
        .| decodeCsvWithP headerValidator p
        .| transPipe lift sinkVector

  pure $ case validatedCsv of
    Left errs -> refute $ NE.fromList $ toList errs
    Right rows -> validation rows

-- | Run a CSV conduit handling invalid UTF8
runCsvConduit
  :: forall r m err
   . MonadUnliftIO m
  => ConduitT () Void (ValidateT (Seq (CsvException err)) (ResourceT m)) r
  -> m (Either (Seq (CsvException err)) r)
runCsvConduit = flip catch nonUtf8 . runResourceT . runValidateT . runConduit
 where
  nonUtf8 (_ :: Conduit.TextException) =
    pure $ Left $ pure CsvUnknownFileEncoding

-- | Stream in 'ByteString's and parse records in constant space
decodeCsv
  :: forall a m err
   . ( MonadThrow m
     , MonadValidate (Seq (CsvException err)) m
     , ValidateHeader a
     , FromNamedRecord a
     )
  => ConduitT ByteString a m ()
decodeCsv = decodeCsvWithP (validateHeader (Proxy @a)) parseNamedRecord

-- | Stream in 'ByteString's and parse records in constant space with a custom
-- record parser
decodeCsvWithP
  :: forall a m err
   . (MonadThrow m, MonadValidate (Seq (CsvException err)) m)
  => (Header -> Validate (NESeq String) ())
  -> (NamedRecord -> Parser a)
  -- ^ Custom record parser
  -> ConduitT ByteString a m ()
decodeCsvWithP headerValidator p =
  Conduit.detectUtf -- Strip any BOMs and decode UTF-*
    .| Conduit.encodeUtf8 -- Cassava needs ByteString
    .| parseCsv headerValidator p

parseCsv
  :: forall a m err
   . MonadValidate (Seq (CsvException err)) m
  => (Header -> Validate (NESeq String) ())
  -> (NamedRecord -> Parser a)
  -> ConduitT ByteString a m ()
parseCsv headerValidator p =
  parseHeader
    headerValidator
    (CsvI.decodeByNameWithP (stripParser p) defaultDecodeOptions)

data CsvException a
  = CsvMissingColumn !Text
  | CsvParseException !Int !Text
  | CsvFileNotFound
  | CsvUnknownFileEncoding
  | -- | A constructor for providing extensible csv exceptions
    CsvExceptionExtension a
  deriving stock (Eq, Show)

instance ToJSON a => ToJSON (CsvException a) where
  toJSON = csvExceptionPairs object toJSON
  toEncoding = csvExceptionPairs (pairs . mconcat) toEncoding

csvExceptionPairs
  :: KeyValue kv => ([kv] -> r) -> (a -> r) -> CsvException a -> r
csvExceptionPairs done extend = \case
  CsvMissingColumn column ->
    done
      [ "message" .= ("Missing column " <> tshow column)
      , "missingColumn" .= column
      ]
  CsvParseException rowNumber message ->
    done ["rowNumber" .= rowNumber, "message" .= message]
  CsvFileNotFound -> done ["message" .= ("file not found" :: Text)]
  CsvUnknownFileEncoding ->
    done ["message" .= ("file could not be decoded" :: Text)]
  CsvExceptionExtension a -> extend a

parseHeader
  :: forall a m err
   . MonadValidate (Seq (CsvException err)) m
  => (Header -> Validate (NESeq String) ())
  -> CsvI.HeaderParser (CsvI.Parser a)
  -> ConduitT ByteString a m ()
parseHeader headerValidator = \case
  CsvI.FailH _ err ->
    lift $ refute $ pure $ CsvParseException headerLineNumber $ pack err
  CsvI.PartialH k ->
    await >>= (parseHeader headerValidator . k) . fromMaybe mempty
  CsvI.DoneH header parser ->
    case runValidate $ headerValidator $ fmap stripUtf8 header of
      Right {} -> parseRow (succ headerLineNumber) parser
      Left errs ->
        lift $ refute $ Seq.fromList $ CsvMissingColumn . pack <$> toList errs

parseRow
  :: MonadValidate (Seq (CsvException err)) m
  => Int
  -> CsvI.Parser a
  -> ConduitT ByteString a m ()
parseRow rowNumber parse = case parse of
  CsvI.Fail _ err ->
    lift $ dispute $ pure $ CsvParseException rowNumber $ pack err
  CsvI.Many rows k -> do
    !newRowNumber <- handleRows rows
    await >>= parseRow newRowNumber . k . fromMaybe mempty
  CsvI.Done rows -> void $ handleRows rows
 where
  handleRows = foldM handleRow rowNumber

handleRow
  :: MonadValidate (Seq (CsvException err)) m
  => Int
  -> Either String a
  -> ConduitT i a m Int
handleRow rowNumber result = do
  either
    (lift . dispute . pure . CsvParseException rowNumber . pack)
    yield
    result
  pure $ rowNumber + 1

-- | Strip leading/trailing whitespace from each key-value pairs
stripNamedRecord :: NamedRecord -> NamedRecord
stripNamedRecord =
  HashMap.fromList . fmap (bimap stripUtf8 stripUtf8) . HashMap.toList

-- | Strip leading/trailing whitespace from @'ByteString'@ via UTF-8
stripUtf8 :: ByteString -> ByteString
stripUtf8 = T.encodeUtf8 . T.strip . T.decodeUtf8

-- | Take a custom parser and return one that strips prior to parsing
stripParser :: (NamedRecord -> Parser a) -> (NamedRecord -> Parser a)
stripParser p = p . stripNamedRecord
