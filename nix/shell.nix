{ nixpkgs }:
nixpkgs.stable.mkShell {
  name = "freckle-app-shell";
  buildInputs =
    with (import ./. { inherit nixpkgs; });
    [
      cabal
      fourmolu
      ghc-with-packages
      haskell-language-server
      haskell-native-dependencies
      stack
    ];
}
