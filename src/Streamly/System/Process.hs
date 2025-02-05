{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.System.Process
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides functions to run operating system processes as stream
-- producers, consumers or stream transformation functions. Thus OS processes
-- can be used in the same way as Haskell functions and all the streaming
-- combinators in streamly can be used to combine them. This allows you to
-- seamlessly integrate external binary executables into your Haskell program.
--
-- However, we recommend native Haskell functions with Streamly threads over
-- using system processes whenever possible. This approach offers a simpler
-- programming model compared to system processes, which also have a larger
-- performance overhead.
--
-- Prefer "Streamly.System.Command" module as a higher level wrapper over this
-- module.
--
-- = Executables as functions
--
-- Processes can be composed in a streaming pipeline just like a Posix shell
-- command pipeline. Moreover, we can mix processes and Haskell functions
-- seamlessly in a processing pipeline. For example:
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
--  & Unicode.decodeLatin1
--  & fmap toUpper
--  & Unicode.encodeLatin1
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
-- = Running Commands Concurrently
--
-- We can run executables or commands concurrently as we would run any other
-- functions in Streamly. For example, the following program greps the word
-- "to" in all the files in the current directory concurrently:
--
-- >>> :{
-- grep file =
--    Process.toBytes "grep" ["-H", "pattern", file]
--  & Stream.handle (\(_ :: Process.ProcessFailure) -> Stream.nil)
--  & Stream.foldMany (Fold.takeEndBy (== 10) Array.write)
--  :}
--
-- >>> :{
-- pgrep =
--    Dir.readFiles "."
--  & Stream.parConcatMap id grep
--  & Stream.fold Stdio.writeChunks
-- :}
--
-- = Running Interactive Programs (e.g. ghci)
--
-- >>> :{
-- ghci =
--  Stdio.readChunks
--   & Process.pipeChunks "ghci" []
--   & Stdio.putChunks
-- :}
--
-- = Experimental APIs
--
-- See "Streamly.Internal.System.Process" for unreleased functions.
--

module Streamly.System.Process
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Exceptions
    -- | Since we are composing using Streamly's streaming pipeline there is
    -- nothing special about exception handling, it works the same as in
    -- Streamly.  Like the @pipefail@ option in shells, exceptions are
    -- propagated if any of the stages fail.
      ProcessFailure (..)

    -- * Process Configuration
    -- | Use the config modifiers to modify the default config.
    , Config

    -- ** Common Modifiers
    -- | These options apply to both POSIX and Windows.
    , setCwd
    , setEnv
    , closeFiles
    , newProcessGroup
    , Session (..)
    , setSession

    -- ** Posix Only Modifiers
    -- | These options have no effect on Windows.
    , interruptChildOnly
    , setUserId
    , setGroupId

    -- ** Windows Only Modifiers
    -- | These options have no effect on Posix.
    , waitForDescendants

    -- ** CleanupConfig options
    , terminateWithSigInt
    , terminateProcessGroup
    , waitOnTermination

    -- * Generation
    , toChunks
    , toChunksWith
    , toBytes
    , toBytesWith
    , toChars
    , toLines

    -- * Effects
    , toString
    , toStdout
    , toNull

    -- * Transformation
    , pipeChunks
    , pipeChunksWith
    , pipeBytes

    -- * Including Stderr Stream
    -- | Like other "Generation" routines but along with stdout, stderr is also
    -- included in the output stream. stdout is converted to 'Right' values in
    -- the output stream and stderr is converted to 'Left' values.
    , toBytesEither
    , toChunksEither
    , toChunksEitherWith
    , pipeBytesEither
    , pipeChunksEither
    , pipeChunksEitherWith

    -- * Non-streaming Processes
    -- | These processes do not attach the IO streams with other processes.
    , foreground
    , daemon
    , standalone

    -- * Deprecated
    , processChunks
    , processBytes
    )
where

import Streamly.Internal.System.Process

#include "DocTestProcess.hs"
