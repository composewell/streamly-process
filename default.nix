# CAUTION! a spelling mistake in arg string is ignored silently.
#
# To use ghc-8.10.7
# nix-shell --argstr compiler "ghc8107"

{
  nixpkgs ?
    import (builtins.fetchTarball
      https://github.com/NixOS/nixpkgs/archive/refs/tags/22.05.tar.gz)
        {}
, compiler ? "ghc922"
}:
let
    utils =
      let src = fetchGit {
            url = "git@github.com:composewell/composewell-haskell.git";
            ref = "master";
          }; in (import "${src}/utils.nix") { inherit nixpkgs; };


    haskellPackages =
      let src = fetchGit {
            url = "git@github.com:composewell/composewell-haskell.git";
            ref = "master";
          }; in (import "${src}/haskellPackages.nix")
            { inherit nixpkgs;
              inherit compiler; };

    mkHaskellPackages = inShell:
      haskellPackages.override (old: {
        overrides =
          nixpkgs.lib.composeExtensions
            (old.overrides or (_: _: {}))
            (with nixpkgs.haskell.lib; self: super: {
                streamly-process =
                  utils.local super "streamly-process" ./. "--benchmark" inShell;
                  streamly = utils.composewell super
                      "streamly"
                      "c2f79142411383e14d8c7db0f2456775549925f0";
                  streamly-core = utils.composewellDir super
                      "streamly"
                      "c2f79142411383e14d8c7db0f2456775549925f0"
                      "/core";
            });
      });

    shellDrv = mkHaskellPackages true;

    shell = utils.mkShell shellDrv (p: [p.streamly-process]) true;

in if nixpkgs.lib.inNixShell
   then shell
   else (mkHaskellPackages false).streamly-process
