-- |
-- Module      : Streamly.System.Process
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Use OS processes as stream transformation functions.
--
-- This module provides functions to run operating system processes as stream
-- source, sink or transformation functions. Thus OS processes can be used in
-- the same way as Haskell functions and all the streaming combinators in
-- streamly can be used to combine them. This allows you to seamlessly
-- integrate external programs into your Haskell program.
--
-- We recommend that you use Streamly threads instead of system processes where
-- possible as they have a simpler programming model and processes have a
-- larger performance overhead.
--
-- = Imports for examples
--
-- Use the following imports for examples below.
--
-- >>> :set -XFlexibleContexts
-- >>> :set -XScopedTypeVariables
-- >>> import Data.Char (toUpper)
-- >>> import Data.Function ((&))
-- >>> import qualified Streamly.Console.Stdio as Stdio
-- >>> import qualified Streamly.Data.Array as Array
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.System.Process as Process
-- >>> import qualified Streamly.Unicode.Stream as Unicode
-- >>> import qualified Streamly.Internal.FileSystem.Dir as Dir
-- >>> import qualified Streamly.Data.Stream.Concurrent as Concur
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
--
-- = Executables as functions
--
-- Streamly provides powerful ways to combine streams. Processes can be
-- composed in a streaming pipeline just like a Posix shell command pipeline
-- except that we use @&@ instead of @|@. Moreover, we can mix processes and
-- Haskell functions seamlessly in a processing pipeline. For example:
--
-- >>> :{
--    Process.toBytes "echo" ["hello world"]
--  & Process.pipeBytes "tr" ["[a-z]", "[A-Z]"]
--  & Stream.fold Stdio.write
--  :}
--  HELLO WORLD
--
-- Of course, you can use a Haskell function instead of "tr":
--
-- >>> :{
--    Process.toBytes "echo" ["hello world"]
--  & Unicode.decodeLatin1 & fmap toUpper & Unicode.encodeLatin1
--  & Stream.fold Stdio.write
--  :}
--  HELLO WORLD
--
-- = Shell commands as functions
--
-- Using a shell as the command interpreter we can use shell commands in a data
-- processing pipeline:
--
-- >>> :{
--    Process.toBytes "sh" ["-c", "echo hello | tr [a-z] [A-Z]"]
--  & Stream.fold Stdio.write
--  :}
--  HELLO
--
-- = Concurrent Processing
--
-- We can run executables or commands concurrently as we would run any other
-- functions in Streamly. For example, the following program greps the word
-- "to" in all the files in the current directory concurrently:
--
-- >>> :{
-- grep file =
--    Process.toBytes "grep" ["-H", "to", file]
--  & Stream.handle (\(_ :: Process.ProcessFailure) -> Stream.nil)
--  & Stream.foldMany (Fold.takeEndBy (== 10) Array.write)
--  :}
--
-- >>> :{
-- pgrep =
--    Dir.readFiles "."
--  & Concur.parConcatMap Concur.eager grep
--  & Stream.fold Stdio.writeChunks
-- :}
--
-- = Exception handling
--
-- Since we are composing using Streamly streaming pipeline there is nothing
-- special about exception handling, it works the same as in Streamly.  Like
-- the @pipefail@ option in shells, exceptions are propagated if any of the
-- stages fail.
--
-- = Process Attributes
--
-- When a new process is spawned, the following attributes are inherited from
-- the parent process:
--
-- * Current working directory
-- * Environment
-- * Open file descriptors
-- * Process group
-- * Process uid and gid
-- * Signal handlers
-- * Terminal (Session)

module Streamly.System.Process
    ( ProcessFailure (..)

    -- * Generation
    , toChunks
    , toBytes

    -- * Transformation
    , pipeChunks
    , pipeBytes

    -- * Deprecated
    , processChunks
    , processBytes
    )
where

import Streamly.Internal.System.Process

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> :set -XScopedTypeVariables
-- >>> import Data.Char (toUpper)
-- >>> import Data.Function ((&))
-- >>> import qualified Streamly.Console.Stdio as Stdio
-- >>> import qualified Streamly.Data.Array as Array
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.System.Process as Process
-- >>> import qualified Streamly.Unicode.Stream as Unicode
-- >>> import qualified Streamly.Internal.FileSystem.Dir as Dir
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
