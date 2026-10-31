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
          default = exif-codec;
        };
        checks = {
          check-default = exif-codec;
        };
        devShells.default = ghc.shellFor {
          packages = ps: [exif-codec];
          nativeBuildInputs = with ghc; [
            haskell-language-server
            implicit-hie
            cabal-install
            cabal-fmt
            pkgs.exiftool
          ];
          buildInputs = [];
          # make hls happy about missing .so files
          shellHook = ''
            export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [exif-codec]}:$LD_LIBRARY_PATH
          '';
        };
      };
    };
}
