{
  inputs = {
    /* The most recent stable channel */
    stable.url =
      "github:nixos/nixpkgs/nixos-23.05";

    /* The unstable channel, the very latest of everything */
    unstable.url =
      "github:nixos/nixpkgs/nixos-unstable";

    /* Hash obtained from https://www.nixhub.io/packages/ghc */
    ghc927.url =
      "github:nixos/nixpkgs/16b3b0c53b1ee8936739f8c588544e7fcec3fc60";

    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgsArgs = {
          inherit system;
          config = { };
        };

        /* An attribute set containing various historic versions of the huge Nix
        package database. This is passed as an argument to functions in the
        ./nix directory which can then select which versions they want to pull
        each dependency from. */

        nixpkgs = {
          stable = import inputs.stable nixpkgsArgs;
          unstable = import inputs.unstable nixpkgsArgs;
          ghc927 = import inputs.ghc927 nixpkgsArgs;
        };

      in
      {
        devShells.default =
          import ./nix/shell.nix { inherit nixpkgs; };
        packages =
          import ./nix { inherit nixpkgs; };
      });
}
