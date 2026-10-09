{
  description = "Halogen Hooks";

  inputs = {
    # nixos-26.05, pinned to an immutable archive.
    nixpkgs.url = "https://github.com/NixOS/nixpkgs/archive/7c8764b7c7b09b34f632464276218ef9090eaa11.tar.gz";
  };

  outputs = { self, nixpkgs, ... }: let
    name = "halogen-hooks";
    supportedSystems = ["aarch64-darwin" "x86_64-darwin" "x86_64-linux"];
  in
    {
      devShells = nixpkgs.lib.genAttrs supportedSystems (system: let
        pkgs = import nixpkgs { inherit system; };
      in {
        default = pkgs.mkShell {
          inherit name;
          packages = [
            pkgs.nodejs_24
            pkgs.git
          ];
          # All PureScript tools use the same npm lockfile as CI.
          shellHook = ''
            export PATH="$PWD/node_modules/.bin:$PATH"
          '';
        };
      });
    };
}
