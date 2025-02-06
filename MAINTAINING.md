# Copy images to haddock

```
$ cabal run --flag use-diagrams Diagram.System.Process
$ cabal haddock

$ export ARCH=x86_64-linux
$ export GHCVER=9.6.6
$ export PKGVER=0.3.1

$ cp -a diagrams/img/ dist-newstyle/build/$ARCH/ghc-$GHCVER/streamly-process-$PKGVER/doc/html/streamly-process/
```
