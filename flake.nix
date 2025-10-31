{
  description = "Emacs Beads - Emacs interface for bd issue tracker";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Eldev is a single shell script from GitHub
        eldev = pkgs.stdenv.mkDerivation {
          pname = "eldev";
          version = "1.11.1";

          src = pkgs.fetchurl {
            url = "https://raw.githubusercontent.com/emacs-eldev/eldev/master/bin/eldev";
            sha256 = "1bvaxq3pvbn9dizj16wg350hyv91qbdlhf26nbzgfd3kbq7w0i1l";
          };

          dontUnpack = true;

          installPhase = ''
            mkdir -p $out/bin
            cp $src $out/bin/eldev
            chmod +x $out/bin/eldev
          '';
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            emacs
            git
            eldev
          ];
        };
      }
    );
}
