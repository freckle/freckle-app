{ nixpkgs }:
let
  inherit (nixpkgs.stable) haskell;
  inherit (haskell.lib) justStaticExecutables overrideCabal;
in
justStaticExecutables
  (haskell.packages.ghc96.override {
    inherit (nixpkgs.unstable) all-cabal-hashes;
    overrides = self: super: {
      fourmolu =
        overrideCabal
          (super.callPackage ./fourmolu.nix { })
          (drv: { doCheck = false; });
    };
  }).fourmolu
