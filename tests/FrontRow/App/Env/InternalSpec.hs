module FrontRow.App.Env.InternalSpec
  ( spec
  )
where

import Prelude

import Control.Applicative
import FrontRow.App.Env
import FrontRow.App.Env.Internal
import Numeric.Natural
import Test.Hspec

spec :: Spec
spec = do
  describe "Parser" $ do
    context "Alternative" $ do
      let
        run :: Parser a -> Either [(String, Error)] a
        run p = unParser p [("PRESENT", "present"), ("INVALID_NAT", "-1")]

      -- Left identity: 'empty <|> x == x'
      it "satisfies left identity" $ do
        run @String (empty <|> var str "PRESENT" mempty)
          `shouldBe` Right "present"

        run @Natural (empty <|> var auto "INVALID_NAT" mempty) `shouldBe` Left
          [("INVALID_NAT", InvalidError "Prelude.read: no parse: \"-1\"")]

        run @String (empty <|> var str "MISSING" mempty)
          `shouldBe` Left [("MISSING", UnsetError)]

      -- Right identity: 'x <|> empty == x'
      it "satisfies right identity" $ do
        run @String (var str "PRESENT" mempty <|> empty)
          `shouldBe` Right "present"

        run @Natural (var auto "INVALID_NAT" mempty <|> empty) `shouldBe` Left
          [("INVALID_NAT", InvalidError "Prelude.read: no parse: \"-1\"")]

        run @String (var str "MISSING" mempty <|> empty)
          `shouldBe` Left [("MISSING", UnsetError)]

      -- Annihilation: 'f <$> empty == empty'
      it "satisfies annihilation" $ do
        run @Natural ((+ 1) <$> empty) `shouldBe` Left []
        run @String ((++ "!") <$> empty) `shouldBe` Left []

      -- Associativity: 'x <|> (y <|> z) == (x <|> y) <|> z'
      it "satisfies associativity" $ do
        run @String (empty <|> (empty <|> empty)) `shouldBe` Left []
        run @String ((empty <|> empty) <|> empty) `shouldBe` Left []

        run @Natural
            (empty
            <|> (var auto "INVALID_NAT" mempty <|> var auto "MISSING" mempty)
            )
          `shouldBe` Left
                       [ ( "INVALID_NAT"
                         , InvalidError "Prelude.read: no parse: \"-1\""
                         )
                       ]
        run @Natural
            ((empty <|> var auto "INVALID_NAT" mempty)
            <|> var auto "MISSING" mempty
            )
          `shouldBe` Left
                       [ ( "INVALID_NAT"
                         , InvalidError "Prelude.read: no parse: \"-1\""
                         )
                       ]

        run @String
            (empty <|> (var str "MISSING" mempty <|> var str "PRESENT" mempty))
          `shouldBe` Right "present"
        run @String
            ((empty <|> var str "MISSING" mempty) <|> var str "PRESENT" mempty)
          `shouldBe` Right "present"

      -- Distributivity: 'f <$> (x <|> y) == (f <$> x) <|> (f <$> y)'
      it "satisfies distributivity" $ do
        run @Natural
            ((+ 1)
            <$> (var auto "NOT_PRESENT" mempty
                <|> var auto "INVALID_NAT" mempty
                )
            )
          `shouldBe` Left
                       [ ("NOT_PRESENT", UnsetError)
                       , ( "INVALID_NAT"
                         , InvalidError "Prelude.read: no parse: \"-1\""
                         )
                       ]

        run @Natural
            ((+ 1)
            <$> var auto "NOT_PRESENT" mempty
            <|> (+ 1)
            <$> var auto "INVALID_NAT" mempty
            )
          `shouldBe` Left
                       [ ("NOT_PRESENT", UnsetError)
                       , ( "INVALID_NAT"
                         , InvalidError "Prelude.read: no parse: \"-1\""
                         )
                       ]

        run @String
            ((++ "!")
            <$> (var str "NOT_PRESENT" mempty <|> var str "PRESENT" mempty)
            )
          `shouldBe` Right "present!"

        run @String
            ((++ "!")
            <$> var str "NOT_PRESENT" mempty
            <|> (++ "!")
            <$> var str "PRESENT" mempty
            )
          `shouldBe` Right "present!"

      -- Left catch: 'pure x <|> empty = pure x'
      it "satisfies left catch"
        $ run (pure True <|> empty)
        `shouldBe` Right True
