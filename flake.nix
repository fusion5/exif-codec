{
  description = "Haskell exif codec.";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    hs-bindgen,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      perSystem = {system, ...}: let
        pkgs = import inputs.nixpkgs {
          inherit system;
        };
        ghc = pkgs.haskell.packages.ghc912.extend (hfinal: hprev: {});
        exif-codec = ghc.callCabal2nix "exif-codec" (pkgs.lib.cleanSource ./.) {};
      in {
        packages = {
        };
        checks = {
        };
        devShells.default = ghc.shellFor {
          packages = ps: [exif-codec];
        };
      };
    };
}
