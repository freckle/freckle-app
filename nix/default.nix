{ nixpkgs }:
{
  cabal = import ./cabal.nix { inherit nixpkgs; };

  fourmolu = import ./fourmolu.nix { inherit nixpkgs; };

  ghc-with-packages = import ./ghc-with-packages.nix { inherit nixpkgs; };

  haskell-language-server = import ./haskell-language-server.nix { inherit nixpkgs; };

  haskell-native-dependencies = import ./haskell-native-dependencies.nix { inherit nixpkgs; };

  stack = import ./stack.nix { inherit nixpkgs; };
}
