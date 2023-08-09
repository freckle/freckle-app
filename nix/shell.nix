{ nixpkgs }:
nixpkgs.stable.mkShell {
  name = "freckle-app-shell";
  buildInputs =
    map (x: import x { inherit nixpkgs; })
      [
        ./cabal.nix
        ./fourmolu.nix
        ./ghc-with-packages.nix
        ./haskell-language-server.nix
        ./haskell-native-dependencies.nix
        ./stack.nix
      ];
}
