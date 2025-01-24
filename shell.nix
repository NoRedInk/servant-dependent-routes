{ pkgs ? import <nixpkgs> { } }:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs {};

in with nixpkgs; 

pkgs.mkShell {
  buildInputs = [
    haskell.compiler.ghc947
    nix-search-cli
    haskellPackages.ghcid
    haskellPackages.cabal-install
    hpack
    hlint
    ormolu
    haskellPackages.haskell-language-server
  ];
}