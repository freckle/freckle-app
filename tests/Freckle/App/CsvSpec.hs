module Freckle.App.CsvSpec
  ( spec
  ) where

import Freckle.App.Prelude

import Conduit (ConduitT, ResourceT, yieldMany)
import Control.Monad.Validate (Validate, refute, runValidate)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Csv hiding (header)
import qualified Data.List.NonEmpty as NE
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Vector as V
import Data.Void (Void)
import Freckle.App.Csv
import Test.Hspec

data Foo = Foo
  { someInt :: Int
  , someText :: Text
  }
  deriving stock (Eq, Show, Generic)

data FooError = BadFoo
  deriving stock (Eq, Show)

instance FromNamedRecord Foo where
  parseNamedRecord = genericParseNamedRecord defaultOptions

instance ToNamedRecord Foo where
  toNamedRecord = genericToNamedRecord defaultOptions

instance DefaultOrdered Foo where
  headerOrder = genericHeaderOrder defaultOptions

instance FromRecord Foo where
  parseRecord = genericParseRecord defaultOptions

instance ToRecord Foo where
  toRecord = genericToRecord defaultOptions

instance ValidateHeader Foo where
  validateHeader = defaultValidateOrderedHeader

spec :: Spec
spec = do
  describe "defaultOptions" $ do
    let
      contentRows = "123,this is some text\r\n456,this is more text\r\n"
      content = "someInt,someText\r\n" <> contentRows
      header = ["someInt", "someText"]
      rows :: [Foo]
      rows = [Foo 123 "this is some text", Foo 456 "this is more text"]
    it "should parse records as expected" $
      decode HasHeader content
        `shouldBe` Right (V.fromList rows)

    it "should parse named records as expected" $
      decodeByName content
        `shouldBe` Right (V.fromList header, V.fromList rows)

    it "should serialize records as expected" $
      encode rows
        `shouldBe` contentRows

    it "should serialize named records as expected" $
      encodeDefaultOrderedByName rows
        `shouldBe` content

  for_ [CustomParser, FromNamedRecordParser] $ \testType -> do
    describe (csvSinkTestName testType) $ do
      it "should generate errors for missing columns" $ do
        let source = yieldMany ["someInt,som_text\n", "3,meep\n", "4,morp\n"]
        result <- csvSinkTest testType source pure
        runValidate result
          `shouldBe` Left (pure $ CsvMissingColumn @Void "someText")

      it "should generate errors for parse errors" $ do
        result <- csvSinkTest testType (yieldMany []) pure
        runValidate result
          `shouldBe` Left
            (pure $ CsvParseException @Void 1 "parse error (not enough input)")

      it "should generate errors per row" $ do
        let
          source =
            yieldMany
              [ "someInt,someText\n"
              , "a,meep\n"
              , "1,flip\n"
              , "b,morp\n"
              , "c,merp\n"
              ]
          errorLine = \case
            CsvParseException i _ -> i
            _ -> -1
        result <- csvSinkTest testType source pure
        first (fmap errorLine) (runValidate result)
          `shouldBe` Left (NE.fromList [2, 4, 5])

      it "should generate errors for non-utf8-encoded files" $ do
        let source = yieldMany [BS.pack [0XFF, 0XFF, 0XFF, 0XFF]]
        result <- csvSinkTest testType source pure
        runValidate result `shouldBe` Left (pure $ CsvUnknownFileEncoding @Void)

      it "should generate errors using user-specified validation" $ do
        let
          validate foo@Foo {..}
            | someInt < 4 = pure foo
            | otherwise = refute $ pure $ CsvExceptionExtension BadFoo
          source =
            yieldMany
              [ "someInt,someText\n"
              , "3,meep\n"
              , "4,morp\n"
              , "1,whomp\n"
              , "5,merp\n"
              ]

        result <- csvSinkTest testType source $ traverse validate
        runValidate result
          `shouldBe` Left
            ( NE.fromList
                [ CsvExceptionExtension BadFoo
                , CsvExceptionExtension BadFoo
                ]
            )

      it "should successfully parse a CSV" $ do
        let
          validate foo@Foo {..}
            | someInt < 4 = pure foo
            | otherwise = refute $ pure $ CsvExceptionExtension BadFoo
          source =
            yieldMany
              ["someInt,someText\n", "1,meep\n", "2,morp\n", "3,merp\n"]

        result <- csvSinkTest testType source $ traverse validate
        runValidate result
          `shouldBe` Right
            (V.fromList [Foo 1 "meep", Foo 2 "morp", Foo 3 "merp"])

      it "should successfully parse a CSV with weird whitespace" $ do
        let
          validate foo@Foo {..}
            | someInt < 4 = pure foo
            | otherwise = refute $ pure $ CsvExceptionExtension BadFoo
          source =
            yieldMany
              [ "someInt\t   ,someText\n"
              , "1, \t meep\n"
              , "2,morp\n"
              , "3 ,  merp\n"
              ]

        result <- csvSinkTest testType source $ traverse validate
        runValidate result
          `shouldBe` Right
            (V.fromList [Foo 1 "meep", Foo 2 "morp", Foo 3 "merp"])

data CSVSinkTest = CustomParser | FromNamedRecordParser

csvSinkTestName :: CSVSinkTest -> String
csvSinkTestName = \case
  CustomParser -> "csvWithParserAndValidationSink"
  FromNamedRecordParser -> "csvWithValidationSink"

customFooHeaderValidator :: Header -> Validate (NESeq String) ()
customFooHeaderValidator h = hasHeader h "someInt" *> hasHeader h "someText"

customFooP :: NamedRecord -> Parser Foo
customFooP r = Foo <$> r .: "someInt" <*> r .: "someText"

csvSinkTest
  :: forall err m
   . (MonadThrow m, MonadUnliftIO m, PrimMonad m)
  => CSVSinkTest
  -> ConduitT () ByteString (ResourceT m) ()
  -> ( Vector Foo
       -> Validate (NonEmpty (CsvException err)) (Vector Foo)
     )
  -> m (Validate (NonEmpty (CsvException err)) (Vector Foo))
csvSinkTest = \case
  CustomParser ->
    csvWithParserAndValidationSink customFooHeaderValidator customFooP
  FromNamedRecordParser -> csvWithValidationSink
