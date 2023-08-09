{ nixpkgs }:
let
  inherit (nixpkgs.stable.lib) fold composeExtensions;
in
fold composeExtensions (_: _: { }) [
  (import ./disabled-haskell-tests.nix { inherit nixpkgs; })
]
