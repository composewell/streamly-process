# OS processes as streams

Use operating system (OS) commands in Haskell programs as if they were
native Haskell functions, by treating their inputs and outputs as
Haskell streams. This allows you to write high-level Haskell scripts
that can perform tasks similar to shell scripts, but with C-like
performance, and with strong safety guarantees, refactorability, and
modularity.

Use the following imports in the examples below:

```haskell
>>> :set -XFlexibleContexts
>>> :set -XScopedTypeVariables
>>> :set -XQuasiQuotes
>>> import Data.Function ((&))
>>> import Streamly.Unicode.String (str)
>>> import qualified Streamly.Data.Array as Array
>>> import qualified Streamly.Console.Stdio as Stdio
>>> import qualified Streamly.Data.Fold as Fold
>>> import qualified Streamly.Data.Stream.Prelude as Stream
>>> import qualified Streamly.System.Command as Command
>>> import qualified Streamly.Internal.FileSystem.Dir as Dir
```

## Commands as streaming functions

The shell command `echo "hello world" | tr [a-z] [A-Z]` can be written as
follows using this package:

```haskell
>>> :{
   Command.toBytes [str|echo "hello world"|] -- Stream IO Word8
 & Command.pipeBytes [str|tr [a-z] [A-Z]|]   -- Stream IO Word8
 & Stream.fold Stdio.write                   -- IO ()
 :}
 HELLO WORLD
```

## Shell commands as streaming functions

If you want to execute the same command using the shell pipe syntax:

```haskell
>>> :{
   Command.toBytes [str|sh "-c" "echo 'hello world' | tr [a-z] [A-Z]"|] -- Stream IO Word8
 & Stream.fold Stdio.write                                              -- IO ()
 :}
 HELLO WORLD
```

## Running Commands Concurrently

You can run commands concurrently by using streamly's concurrent combinators.

Running @grep@ concurrently on many files:

```haskell
>>> :{
grep file =
   Command.toBytes [str|grep -H "pattern" #{file}|]             -- Stream IO Word8
 & Stream.handle (\(_ :: Command.ProcessFailure) -> Stream.nil) -- Stream IO Word8
 & Stream.foldMany (Fold.takeEndBy (== 10) Array.write)         -- Stream IO (Array Word8)
 :}

>>> :{
pgrep =
   Dir.readFiles "."             -- Stream IO FilePath
 & Stream.parConcatMap id grep   -- Stream IO (Array Word8)
 & Stream.fold Stdio.writeChunks -- IO ()
:}
```

## Streamly

Please visit [Streamly homepage](https://streamly.composewell.com) for more
details about streamly.

# Licensing

Available under [Apache-2.0 license](LICENSE).
