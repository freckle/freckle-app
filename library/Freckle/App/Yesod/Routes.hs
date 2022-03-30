{-# LANGUAGE CPP #-}

module Freckle.App.Yesod.Routes
  ( mkRouteNameCaseExp
  ) where

import Freckle.App.Prelude

import qualified Language.Haskell.TH as TH
import Yesod.Routes.TH.Types

-- | Lambdacase expression to print route names
--
-- It has the following type:
--
-- > _ :: Route a -> String
--
-- It produces code like:
--
-- > \case
-- >   RoutePiece a -> case a of
-- >     RouteResource{} -> "ResourceName"
--
mkRouteNameCaseExp :: [ResourceTree String] -> TH.Q TH.Exp
mkRouteNameCaseExp tree = TH.LamCaseE . fold <$> traverse mkMatches tree

-- | Make match expressions for a big case over routes
--
-- > RoutePiece a -> case a of
-- >   ...
--
mkMatches :: ResourceTree String -> TH.Q [TH.Match]
mkMatches (ResourceLeaf resource) = pure [mkLeafMatch resource]
mkMatches (ResourceParent name _checkOverlap params children) = do
  caseVar <- TH.newName "a"
  let
    -- by convention the final param in a route is the next route constructor
    paramVars =
      fmap (const TH.WildP) (filter isDynamic params) <> [TH.VarP caseVar]
  matches <- fold <$> traverse mkMatches children
  pure
    [ TH.Match
        (conP constName paramVars)
        (TH.NormalB $ TH.CaseE (TH.VarE caseVar) matches)
        []
    ]
  where constName = TH.mkName name

conP :: TH.Name -> [TH.Pat] -> TH.Pat
#if MIN_VERSION_template_haskell(2,18,0)
conP x = TH.ConP x []
#else
conP = TH.ConP
#endif

isDynamic :: Piece a -> Bool
isDynamic = \case
  Static{} -> False
  Dynamic{} -> True

-- | Leaf match expressions for a resource
--
-- > Name{} -> "ResourceName"
--
mkLeafMatch :: Resource String -> TH.Match
mkLeafMatch resource = TH.Match
  (TH.RecP constName [])
  (TH.NormalB $ TH.LitE $ TH.StringL name)
  []
 where
  constName = TH.mkName name
  name = resourceName resource
