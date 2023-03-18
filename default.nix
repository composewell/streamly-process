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
                  streamly-core =
                      super.callHackageDirect
                        { pkg = "streamly-core";
                          ver = "0.1.0";
                          sha256 = "hoSV6Q2+X5a7hFnJAArqNPjcMaCVyX9Vz4FcxeJ+jgI=";
                        } {};
                  streamly =
                    nixpkgs.haskell.lib.overrideCabal
                      (super.callHackageDirect
                        { pkg = "streamly";
                          ver = "0.9.0";
                          sha256 = "sha256-eOxVb8qQjZDo1+S7CStqYSExOg2QHWkMY+zlOYqwZak=";
                        } {})
                    #  (let src = fetchGit {
                    #      url = "git@github.com:composewell/streamly.git";
                    #      rev = "96d222e45cf3aee9b6847c0d14fde967a760fee8";
                    #  }; in super.callCabal2nix "streamly" src {})
                      (old:
                        { librarySystemDepends =
                            if nixpkgs.lib.strings.hasInfix "darwin" builtins.currentSystem
                            then [nixpkgs.darwin.apple_sdk.frameworks.Cocoa]
                            else [];
                        });

            });
      });

    shellDrv = mkHaskellPackages true;

    shell = utils.mkShell shellDrv (p: [p.streamly-process]) true;

in if nixpkgs.lib.inNixShell
   then shell
   else (mkHaskellPackages false).streamly-process
