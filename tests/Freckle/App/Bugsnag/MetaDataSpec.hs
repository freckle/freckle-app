module Freckle.App.Bugsnag.MetaDataSpec
  ( spec
  ) where

import Freckle.App.Test

import Blammo.Logging
import Data.Aeson
import Data.Bugsnag
import Freckle.App.Bugsnag
import Freckle.App.Bugsnag.MetaData
import Freckle.App.Stats
import qualified Freckle.App.Stats as Stats
import UnliftIO.Exception (SomeException)

spec :: Spec
spec = do
  describe "collectMetaData" $ do
    it "collects from StatsClient and myThreadContext" $ example $ do
      let settings = setStatsSettingsTags [("foo", "bar")] defaultStatsSettings

      withStatsClient settings $ \client -> flip runReaderT client $ do
        Stats.tagged [("baz", "bat")] $ do
          withThreadContext ["quix" .= ("quip" :: Text)] $ do
            collected <- collectMetaData

            let
              expected = mconcat
                [ metaData
                  "tags"
                  ["foo" .= ("bar" :: Text), "baz" .= ("bat" :: Text)]
                , metaData "context" ["quix" .= ("quip" :: Text)]
                ]

            runMergeMetaData Nothing collected `shouldBe` Just expected

  describe "mergeMetaData" $ do
    it "adds metadata to an empty Event" $ example $ do
      let incoming = metaData "test" ["foo" .= ("bar" :: Text)]

      runMergeMetaData Nothing incoming `shouldBe` Just incoming

    it "preserves new metadata on collisions" $ example $ do
      let
        existing = metaData
          "test"
          [ "foo" .= ("bar1" :: Text)
          , "baz" .= object ["bat" .= ("quix1" :: Text)]
          , "keep" .= ("me" :: Text)
          ]
        incoming = metaData
          "test"
          [ "foo" .= ("bar2" :: Text)
          , "baz" .= object ["bat" .= ("quix2" :: Text)]
          , "add" .= ("me" :: Text)
          ]
        expected = metaData
          "test"
          [ "foo" .= ("bar2" :: Text)
          , "baz" .= object ["bat" .= ("quix2" :: Text)]
          , "keep" .= ("me" :: Text)
          , "add" .= ("me" :: Text)
          ]


      runMergeMetaData (Just existing) incoming `shouldBe` Just expected

runMergeMetaData :: Maybe MetaData -> MetaData -> Maybe MetaData
runMergeMetaData mExisting incoming = MetaData <$> event_metaData event
 where
  event = runBeforeNotify (mergeMetaData incoming) someException
    $ defaultEvent { event_metaData = unMetaData <$> mExisting }

someException :: SomeException
someException = undefined
