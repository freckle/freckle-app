{ nixpkgs }:
nixpkgs.stable.mkShell {
  name = "freckle-app-shell";
  packages =
    with (import ./. { inherit nixpkgs; });
    [
      cabal
      fourmolu
      ghc-with-packages
      haskell-language-server
      haskell-native-dependencies
      stack
    ];
  shellHook = ''
    export STACK_YAML=stack-lts-20.26.yaml
  '';
}
