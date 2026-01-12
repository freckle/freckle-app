module Freckle.App.Test.Hspec.AnnotatedException
  ( unwrapAnnotatedHUnitFailure
  , annotateHUnitFailure
  ) where

import Freckle.App.Prelude

import Control.Exception qualified
import Control.Lens (Lens', lens, over)
import Data.Annotation (Annotation, tryAnnotations)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Text qualified as T
import Freckle.App.Exception (AnnotatedException (..))
import GHC.Stack (CallStack, prettyCallStack)
import Prettyprinter
import Prettyprinter.Render.String
import Prettyprinter.Util (reflow)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec

-- | An hspec hook that lets hspec catch and pretty-print 'HUnitFailure', the
--   exception that is thrown when a test assertion fails
--
-- Tests for any code that might throw 'AnnotatedException' (which includes anything
-- that uses freckle-app) should add this hook to their test suite. Without it, if
-- you end up with an @'AnnotatedException' 'HUnitFailure'@, hspec doesn't recognize
-- it as an assertion failure and you get ugly output instead of nice output.
unwrapAnnotatedHUnitFailure :: Spec -> Spec
unwrapAnnotatedHUnitFailure = around_ $ mapException annotateHUnitFailure

mapException :: (Exception e, Exception e') => (e -> e') -> IO a -> IO a
mapException f = Control.Exception.handle $ Control.Exception.throw . f

annotateHUnitFailure :: AnnotatedException HUnitFailure -> HUnitFailure
annotateHUnitFailure
  AnnotatedException {exception, annotations} =
    over hUnitFailureReason (annotateFailureReason annotations) exception

hUnitFailureReason :: Lens' HUnitFailure FailureReason
hUnitFailureReason =
  lens
    (\(HUnitFailure _ x) -> x)
    (\(HUnitFailure l _) x -> HUnitFailure l x)

-- | Augment a 'FailureReason' with extra information derived from 'Annotation's
annotateFailureReason :: [Annotation] -> FailureReason -> FailureReason
annotateFailureReason as =
  \case
    Reason m -> Reason (makeMessage m as)
    ExpectedButGot m e g -> ExpectedButGot (makeMessageMaybe m as) e g

-- | Construct a message that consists of an introductory paragraph plus
--   some additional paragraphs based on annotations, separated by blank lines
makeMessage :: String -> [Annotation] -> String
makeMessage m as =
  combineParagraphs $ pretty m :| annotationParagraphs as

-- | Like 'makeMessage' but without necessarily having an introductory paragraph present
--
-- If there is neither an introductory paragraph nor any annotations, the result is 'Nothing'.
makeMessageMaybe :: Maybe String -> [Annotation] -> Maybe String
makeMessageMaybe mm as =
  fmap combineParagraphs
    $ nonEmpty
    $ fmap pretty (toList mm) <> annotationParagraphs as

-- | Combine a list of paragraphs into a single string for the final output
combineParagraphs :: Foldable t => t (Doc ann) -> String
combineParagraphs =
  renderString
    . layoutSmart defaultLayoutOptions
    . vsep
    . punctuate "\n"
    . toList

-- | Render a list of annotations as a list of paragraphs
--
-- The paragraphs, depending on how much information there is to display, are:
--
-- * a summary of any annotations that aren't call stacks, if any
-- * the first call stack, if there are any call stacks
annotationParagraphs :: [Annotation] -> [Doc ann]
annotationParagraphs annotations =
  catMaybes
    [ otherAnnotationsPart <$> nonEmpty otherAnnotations
    , callStackPart <$> listToMaybe callStacks
    ]
 where
  (callStacks, otherAnnotations) = tryAnnotations @CallStack annotations

-- | Construct a paragraph consisting of a bullet list of annotations
otherAnnotationsPart :: Foldable t => t Annotation -> Doc ann
otherAnnotationsPart =
  vsep
    . ("Annotations:" :)
    . fmap (indent 2 . ("*" <+>) . align . reflow . T.pack . show)
    . toList

-- | Construct a paragraph that displays a call stack
callStackPart :: CallStack -> Doc ann
callStackPart = pretty . prettyCallStack
