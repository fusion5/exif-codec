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
        exifSamplesSource = pkgs.fetchFromGitHub {
          owner = "ianare";
          repo = "exif-samples";
          rev = "master";
          hash = "sha256-imVQZnG+WmzHBM0Gqj8+fG96h2PAxKkH2JNC/Ban3jw=";
        };
        exifSamplesDir = pkgs.stdenv.mkDerivation {
          name = "exif-samples";
          src = "${exifSamplesSource}/jpg";
          nativeBuildInputs = [pkgs.exiftool];
          installPhase = ''
            mkdir -p $out

            # Recursively find all Jpegs, extract their basename, and write directly to $out
            find . -type f \( -name "*.jpg" -o -name "*.jpeg" -o -name "*.JPG" -o -name "*.JPEG" \) | while read -r file; do
              flatname=$(echo "$file" | sed 's|^\./||' | tr '/' '_')
              base="''${flatname%.*}"

              # Attempt to extract EXIF. If it fails (no metadata), clean up and skip.
              if exiftool -o "$out/$base.exif" -exif:all "$file" 2>/dev/null; then
                true
              else
                rm -f "$out/$base.exif"
              fi
            done
          '';
        };
      in {
        packages = {
          default = exif-codec;
        };
        checks = {
          check-default = exif-codec;
        };
        devShells.default = ghc.shellFor {
          env = {
            EXIF_SAMPLES_DIR = "${exifSamplesDir}";
          };
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
          # shellHook = ''
          #   export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [exif-codec]}:$LD_LIBRARY_PATH
          # '';
        };
      };
    };
}
