{ pkgs ? import <nixpkgs> {} }:
with pkgs;
stdenv.mkDerivation {
  name = "pie-dev-env";
  buildInputs = [
    haskellPackages.tasty-discover
  ];
}
