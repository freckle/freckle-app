/*

This package contains GHC and a bunch of Haskell packages.

It is important for the GHC version to match exactly what is expected by our
current Stack resolver.

It is less important for the packages to stay in sync with Stack, because if
Stack doesn't find what it wants in the shell environment, Stack will happily
build the dependencies it wants on its own. However, the better the nix shell
can stay in sync with Stack, the more benefit we can get out of it in terms of
build reproducibility and being able to utilize the nix binary cache for speed.

*/
{ nixpkgs }:
let
  haskellPackages = nixpkgs.ghc927.haskellPackages.override {
    inherit (nixpkgs.unstable) all-cabal-hashes;
    overrides = import ./haskell-package-overrides.nix
      { inherit nixpkgs; };
  };
in
haskellPackages.ghcWithPackages (import ./haskell-package-selection.nix)
