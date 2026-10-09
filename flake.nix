{
  description = "Halogen Hooks";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-22.05";
    nodejs-nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nodejs-nixpkgs, easy-purescript-nix, flake-utils, ... }: let
    name = "halogen-hooks";
    supportedSystems = ["aarch64-darwin" "x86_64-darwin" "x86_64-linux"];
  in
    flake-utils.lib.eachSystem supportedSystems (
      system: let
        pkgs = import nixpkgs {inherit system;};
        nodePkgs = import nodejs-nixpkgs {inherit system;};
        pursPkgs = import easy-purescript-nix {inherit pkgs;};
      in {
        devShells = {
          default = pkgs.mkShell {
            inherit name;
            packages = [
              nodePkgs.nodejs_22
              pursPkgs.purs-0_15_4
              pursPkgs.purs-tidy
            ];
            # Spago and esbuild use the same npm lockfile as CI.
            shellHook = ''
              export PATH="$PWD/node_modules/.bin:$PATH"
            '';
          };
        };
      }
    );
}
