let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  haskellPackages = pkgs.haskell.packages.ghc9124.override (old: {
    overrides = haskellSelf: haskellSuper:
      let
        hlsSubPkg = name: haskellSelf.callCabal2nix name "${sources.haskell-language-server-2_14_0_0}/${name}" { };
      in
        {
          # support for hls + ghc 9.12.4; building without optimizations
          # is a good workaround for `lookupIdSubst` panic during nix build
          # (ghcide) as well as `Iface id out of scope: ww` (hls-test-utils).
          # we shouldn't need to do all of this in nixpkgs >= 26.11 as long as
          # hls >= 2.14.0.0 is provided
          hie-bios = pkgs.haskell.lib.dontCheck haskellSelf.hie-bios_0_19_0;
          hiedb = haskellSelf.hiedb_0_8_0_0;
          lsp = haskellSelf.lsp_2_8_0_0;
          lsp-test = pkgs.haskell.lib.dontCheck haskellSelf.lsp-test_0_18_0_0;
          ghcide = pkgs.haskell.lib.compose.disableOptimization (hlsSubPkg "ghcide");
          hls-graph = hlsSubPkg "hls-graph";
          hls-plugin-api = pkgs.haskell.lib.dontCheck (hlsSubPkg "hls-plugin-api");
          hls-test-utils = pkgs.haskell.lib.compose.disableOptimization (hlsSubPkg "hls-test-utils");
          unordered-containers = haskellSelf.unordered-containers_0_2_21;

          # tell these packages to use the `Cabal-syntax` package built into
          # ghc rather than the one coming from nixpkgs
          ormolu = haskellSuper.ormolu.override {
            Cabal-syntax = null;
          };
          fourmolu = haskellSuper.fourmolu.override {
            Cabal-syntax = null;
          };

          # skip tests; the built binaries between plugin projects can't be
          # found in the nix sandbox (todo: fix, or just wait for nixpkgs >= 26.11)
          #
          # build without `dynamic` flag for package so that we avoid an issue
          # where the `/build` sandbox path is referenced within the executable
          haskell-language-server = pkgs.haskell.lib.disableCabalFlag (pkgs.haskell.lib.dontCheck (
            haskellSelf.callCabal2nix "haskell-language-server" sources.haskell-language-server-2_14_0_0 {
              Cabal-syntax = null;
            }
          )) "dynamic";
        };
  });

in with pkgs; 

pkgs.mkShell {
  buildInputs = [
    haskellPackages.ghc
    nix-search-cli
    haskellPackages.ghcid
    haskellPackages.cabal-install
    hpack
    hlint
    haskellPackages.ormolu
    haskellPackages.haskell-language-server
    zlib
  ];
}