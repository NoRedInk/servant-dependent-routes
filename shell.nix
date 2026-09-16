let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  haskellPackages = pkgs.haskell.packages.ghc9124.override (old: {
    overrides =
      haskellSelf: haskellSuper:
      let
        hlsPkgSrc =
          {
            subPkg ? null,
            version ? "2.14.0.0",
          }:
          {
            inherit version;
            src =
              if subPkg == null then
                sources.haskell-language-server-2_14_0_0
              else
                "${sources.haskell-language-server-2_14_0_0}/${subPkg}";
          };
      in
      {
        # support for hls + ghc 9.12.4; we shouldn't need to do all of this
        # in nixpkgs >= 26.11 as long as hls >= 2.14.0.0 is provided.
        # optimizations are disabled for some packages as a workaround to
        # avoid e.g. `lookupIdSubst` panic (ghcide), `Iface id out of scope:  ww`
        # (hls-test-utils)
        hie-bios = pkgs.haskell.lib.dontCheck haskellSelf.hie-bios_0_19_0;
        hiedb = haskellSelf.hiedb_0_8_0_0;
        lsp = haskellSelf.lsp_2_8_0_0;
        lsp-test = pkgs.haskell.lib.dontCheck haskellSelf.lsp-test_0_18_0_0;
        unordered-containers = haskellSelf.unordered-containers_0_2_21;
        ghcide = pkgs.haskell.lib.disableOptimization (
          pkgs.haskell.lib.overrideSrc haskellSuper.ghcide (hlsPkgSrc {
            subPkg = "ghcide";
          })
        );
        hls-graph = pkgs.haskell.lib.overrideSrc haskellSuper.hls-graph (hlsPkgSrc {
          subPkg = "hls-graph";
        });
        hls-plugin-api = pkgs.haskell.lib.addBuildTools (pkgs.haskell.lib.overrideSrc
          haskellSuper.hls-plugin-api
          (hlsPkgSrc {
            subPkg = "hls-plugin-api";
          })
        ) [ pkgs.git ]; # for tests
        hls-test-utils = pkgs.haskell.lib.disableOptimization (
          pkgs.haskell.lib.overrideSrc haskellSuper.hls-test-utils (hlsPkgSrc {
            subPkg = "hls-test-utils";
          })
        );
        haskell-language-server = pkgs.haskell.lib.overrideSrc haskellSuper.haskell-language-server (
          hlsPkgSrc { }
        );
      };
  });

in
with pkgs;

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
    nixfmt
    zlib
  ];
}
