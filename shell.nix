{ nixpkgs ? import <nixpkgs> {} }:
with nixpkgs;
stdenv.mkDerivation {
  name = "pie-dev-env";
  buildInputs = [
    haskellPackages.tasty-discover
  ];
}
