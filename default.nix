# CAUTION! a spelling mistake in arg string is ignored silently.
#
# To use ghc-8.6.5
# nix-shell --argstr compiler "ghc865"

{
  nixpkgs ?
    import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/tags/21.05.tar.gz)
        {}
, compiler ? "default"
, c2nix ? "" # cabal2nix CLI options
# TODO
#, sources ? [] # e.g. [./. ./benchmark]
#, hdeps ? [] # e.g. [time, mtl]
#, deps ? [] # e.g. [SDL2]
}:
let haskellPackages =
        if compiler == "default"
        then nixpkgs.haskellPackages
        else nixpkgs.haskell.packages.${compiler};

    # we can possibly avoid adding our package to HaskellPackages like
    # in the case of nix-shell for a single package?
    mkPackage = super: pkg: path: opts: inShell:
                let orig = super.callCabal2nixWithOptions pkg path opts {};
                 in if inShell
                    # Avoid copying the source directory to nix store by using
                    # src = null.
                    then orig.overrideAttrs (oldAttrs: { src = null; })
                    else orig;

    flags = "--benchmark --flag fusion-plugin" + " " + c2nix;

    mkHaskellPackages = inShell:
        haskellPackages.override {
            overrides = self: super:
                with nixpkgs.haskell.lib;
                {
                    streamly-process = mkPackage super "streamly-process" ./. flags inShell;

                    # TODO: Uncomment the following after the streamly release

                    # streamly =
                    #   nixpkgs.haskell.lib.overrideCabal
                    #     (super.callHackageDirect
                    #       { pkg = "streamly";
                    #         ver = "0.8.0";
                    #         sha256 = "0vy2lkljizlhpbpbybmg9jcmj2g4s1aaqd2dzy5c0y0n4rgwxask";
                    #       } {})
                    #     (old:
                    #       { librarySystemDepends =
                    #           if builtins.currentSystem == "x86_64-darwin"
                    #           then [nixpkgs.darwin.apple_sdk.frameworks.Cocoa]
                    #           else [];
                    #         enableLibraryProfiling = false;
                    #         doHaddock = false;

                    # unicode-data =
                    #   super.callHackageDirect
                    #     { pkg = "unicode-data";
                    #       ver = "0.2.0";
                    #       sha256 = "073wbhdxj1sh51907laihbzkkhabs8s71pqhag16lvmgbb7a3hla";
                    #     } {};

                    tasty-bench =
                      super.callHackageDirect
                        { pkg = "tasty-bench";
                          ver = "0.3";
                          sha256 = "0na1q52zr8p1zz8hby4242yjr2zma3js4n91avl7ibsa2y51vrc4";
                        } {};
                };
        };

    drv = mkHaskellPackages true;

    shell = drv.shellFor {
        packages = p:
          [ p.streamly-process
          ];
        doBenchmark = true;
        # Use a better prompt
        shellHook = ''
          export CABAL_DIR="$(pwd)/.cabal.nix"
          if test -n "$PS_SHELL"
          then
            export PS1="$PS_SHELL\[$bldred\](nix)\[$txtrst\] "
          fi
        '';
    };
in if nixpkgs.lib.inNixShell
   then shell
   else (mkHaskellPackages false).streamly-process
