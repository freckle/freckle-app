module Freckle.App.Test.Hspec.AnnotatedExceptionSpec
  ( spec
  ) where

import Freckle.App.Prelude

import Data.Annotation (toAnnotation)
import Freckle.App.Exception (AnnotatedException (..))
import Freckle.App.Test.Hspec.AnnotatedException (annotateHUnitFailure)
import GHC.Exts (fromList)
import GHC.Stack (CallStack, SrcLoc (..))
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "annotateHUnitFailure" $ do
    describe "does nothing if there are no annotations" $ do
      it "when the failure is Reason" $
        let e = HUnitFailure Nothing (Reason "x")
        in  annotateHUnitFailure (AnnotatedException [] e) `shouldBe` e

      it "when the failure is ExpectedButGot with no message" $
        let e = HUnitFailure Nothing (ExpectedButGot Nothing "a" "b")
        in  annotateHUnitFailure (AnnotatedException [] e) `shouldBe` e

      it "when the failure is ExpectedButGot with a message" $
        let e = HUnitFailure Nothing (ExpectedButGot (Just "x") "a" "b")
        in  annotateHUnitFailure (AnnotatedException [] e) `shouldBe` e

    describe "can show an annotation" $ do
      it "when the failure is Reason" $
        annotateHUnitFailure
          AnnotatedException
            { annotations = [toAnnotation @Int 56]
            , exception = HUnitFailure Nothing (Reason "x")
            }
          `shouldBe` HUnitFailure
            Nothing
            ( Reason
                "x\n\
                \\n\
                \Annotations:\n\
                \\t * Annotation @Int 56"
            )

      it "when the failure is ExpectedButGot with no message" $ do
        annotateHUnitFailure
          AnnotatedException
            { annotations = [toAnnotation @Int 56]
            , exception = HUnitFailure Nothing (ExpectedButGot Nothing "a" "b")
            }
          `shouldBe` HUnitFailure
            Nothing
            ( ExpectedButGot
                ( Just
                    "Annotations:\n\
                    \\t * Annotation @Int 56"
                )
                "a"
                "b"
            )

      it "when the failure is ExpectedButGot with a message" $
        annotateHUnitFailure
          AnnotatedException
            { annotations = [toAnnotation @Int 56]
            , exception = HUnitFailure Nothing (ExpectedButGot (Just "x") "a" "b")
            }
          `shouldBe` HUnitFailure
            Nothing
            ( ExpectedButGot
                ( Just
                    "x\n\
                    \\n\
                    \Annotations:\n\
                    \\t * Annotation @Int 56"
                )
                "a"
                "b"
            )

    it "can show a stack trace" $
      annotateHUnitFailure
        AnnotatedException
          { annotations =
              [ toAnnotation @CallStack $
                  fromList
                    [
                      ( "abc"
                      , SrcLoc
                          { srcLocPackage = "thepackage"
                          , srcLocModule = "Foo"
                          , srcLocFile = "src/Foo.hs"
                          , srcLocStartLine = 7
                          , srcLocStartCol = 50
                          , srcLocEndLine = 8
                          , srcLocEndCol = 23
                          }
                      )
                    ]
              ]
          , exception = HUnitFailure Nothing (Reason "x")
          }
        `shouldBe` HUnitFailure
          Nothing
          ( Reason
              "x\n\
              \\n\
              \CallStack (from HasCallStack):\n\
              \  abc, called at src/Foo.hs:7:50 in thepackage:Foo"
          )

    it "can show both an annotation and a stack trace" $
      annotateHUnitFailure
        AnnotatedException
          { annotations =
              [ toAnnotation @Text "Visibility is poor"
              , toAnnotation @CallStack $
                  fromList
                    [
                      ( "abc"
                      , SrcLoc
                          { srcLocPackage = "thepackage"
                          , srcLocModule = "Foo"
                          , srcLocFile = "src/Foo.hs"
                          , srcLocStartLine = 7
                          , srcLocStartCol = 50
                          , srcLocEndLine = 8
                          , srcLocEndCol = 23
                          }
                      )
                    ]
              ]
          , exception = HUnitFailure Nothing (Reason "x")
          }
        `shouldBe` HUnitFailure
          Nothing
          ( Reason
              "x\n\
              \\n\
              \Annotations:\n\
              \\t * Annotation @Text \"Visibility is poor\"\n\
              \\n\
              \CallStack (from HasCallStack):\n\
              \  abc, called at src/Foo.hs:7:50 in thepackage:Foo"
          )
