/*

  This package provides native dependencies of Haskell packages.

  If Stack gives you an error message like...

    - setup: The program '...' is required but it could not be found.
    - The pkg-config package '...' is required but it could not be found.
    - Missing dependency on a foreign library: Missing (or bad) C library: ...

  ... then you probably need to add something to this file.

  To test that a Haskell package actually builds correctly within the nix
  shell environment, it can help to first remove the package from the cache.
  You can do that by running this command:

      stack exec -- ghc-pkg unregister --force <haskell package name>

  Then run `stack build <haskell package name>` to recompile.

*/
{ nixpkgs }:
let
  pkgs = nixpkgs.stable;
in
pkgs.symlinkJoin {
  name = "freckle-app-native-deps";
  paths = [
    pkgs.pcre
    pkgs.pcre.dev
    pkgs.postgresql
    pkgs.rdkafka
    pkgs.zlib
    pkgs.zlib.dev
  ];
}
