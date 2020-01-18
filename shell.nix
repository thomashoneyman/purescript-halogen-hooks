let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz";
  }) {};

  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    # 2019-07-06
    rev = "a09d4ff6a8e4a8a24b26f111c2a39d9ef7fed720";
    sha256 = "1iaid67vf8frsqfnw1vm313d50mdws9qg4bavrhfhmgjhcyqmb52";
  }) {};

in pkgs.stdenv.mkDerivation {
  name = "halogen-hooks";
  buildInputs = with easy-ps; [
    purs spago purty pscid
    pkgs.yarn pkgs.nodejs-12_x
  ];
}
