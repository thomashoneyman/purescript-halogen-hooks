let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz";
  }) {};

  # 2020-03-16 nix-prefetch-git https://github.com/justinwoo/easy-purescript-nix
  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "aa3e608608232f4a009b5c132ae763fdabfb4aba";
    sha256 = "0y6jikncxs9l2zgngbd1775f1zy5s1hdc5rhkyzsyaalcl5cajk8";
  }) {};

in pkgs.stdenv.mkDerivation {
  name = "halogen-hooks";
  buildInputs = with pursPkgs; [
    purs spago purty pscid
    pkgs.yarn pkgs.nodejs-12_x
  ];
}
