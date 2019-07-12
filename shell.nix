{ pkgs ? import ./nix {} }:
with pkgs;
stdenv.mkDerivation {
  name = "pie-dev-env";
  buildInputs = [
    pkgs.niv
    pkgs.ghc
    pkgs.cabal-install
  ];
}
