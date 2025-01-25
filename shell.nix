{ pkgs ? import <nixpkgs> { } }:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs {};

in with nixpkgs; 

pkgs.mkShell {
  buildInputs = [
    haskellPackages.ghc
    nix-search-cli
    haskellPackages.ghcid
    haskellPackages.cabal-install
    hpack
    hlint
    ormolu
    haskellPackages.haskell-language-server
    zlib
  ];
}