{
  description = "Halogen Hooks";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-22.05";
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, easy-purescript-nix, flake-utils, ... }: let
    name = "halogen-hooks";
    supportedSystems = ["aarch64-darwin" "x86_64-darwin" "x86_64-linux"];
  in
    flake-utils.lib.eachSystem supportedSystems (
      system: let
        pkgs = import nixpkgs {inherit system;};
        pursPkgs = import easy-purescript-nix {inherit pkgs;};
      in {
        devShells = {
          default = pkgs.mkShell {
            inherit name;
            packages = [
              pkgs.nodejs-16_x
              pkgs.esbuild

              pkgs.nodePackages.bower

              pursPkgs.purs
              pursPkgs.spago
              pursPkgs.purs-tidy
            ];
          };
        };
      }
    );
}
