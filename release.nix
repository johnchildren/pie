{ pkgs ? import <nixpkgs> {}, compiler ? "ghc864" }:
{
  pie = pkgs.haskell.packages.${compiler}.callPackage ./default.nix {};
}
