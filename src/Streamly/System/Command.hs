{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.System.Command
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides a way to invoke external executables and use them
-- seamlessly in a Haskell program, in a streaming fashion. This enables you to
-- write high-level Haskell scripts to perform tasks similar to shell scripts
-- without requiring the shell. Moreover, Haskell scripts provide C-like
-- performance.
--
-- Please see the "Streamly.System.Process" for basics. This module is a
-- wrapper over that module. "Streamly.System.Process" requires
-- specifying a command executable name and its arguments separately (e.g.
-- "ls" "-al") whereas using this module we can specify the executable and its
-- arguments more conveniently as a single command string e.g.  we can execute
-- "ls -al".
--
-- A command string is parsed in the same way as a posix shell would parse it.
-- A command string consists of whitespace separated tokens with the first
-- token treated as the executable name and the rest as arguments. Whitespace
-- can be escaped using @\\@. Alternatively, double quotes or single quotes can
-- be used to enclose tokens with whitespaces. Quotes can be escaped using @\\@.
-- Single quotes inside double quotes or vice-versa are treated as normal
-- characters.
--
-- You can use the string quasiquoter 'Streamly.Unicode.String.str' to write
-- commands conveniently, it allows Haskell variable expansion as well e.g.:
--
-- >>> f = "file name"
-- >>> [str|ls -al "#{f} with spaces"|]
-- "ls -al \"file name with spaces\""
--
-- With the "Streamly.System.Command" module you can write the examples in the
-- "Streamly.System.Process" module more conveniently.
--
-- = Executables as functions
--
-- The shell command @echo "hello world" | tr [a-z] [A-Z]@ can be written as
-- follows using this module:
--
-- >>> :{
--    Command.toBytes [str|echo "hello world"|]
--  & Command.pipeBytes [str|tr [a-z] [A-Z]|]
--  & Stream.fold Stdio.write
--  :}
--  HELLO WORLD
--
-- = Shell commands as functions
--
-- We recommend using streamly to compose commands natively in Haskell rather
-- than using the shell as shown in the previous example. However, if for some
-- reason you want to execute commands using the shell:
--
-- >>> :{
--    Command.toBytes [str|sh "-c" "echo 'hello world' | tr [a-z] [A-Z]"|]
--  & Stream.fold Stdio.write
--  :}
--  HELLO WORLD
--
-- = Running Commands Concurrently
--
-- This example shows the power of composing in Haskell rather than using the
-- shell. Running @grep@ concurrently on many files:
--
-- >>> :{
-- grep file =
--    Command.toBytes [str|grep -H "pattern" #{file}|]
--  & Stream.handle (\(_ :: Command.ProcessFailure) -> Stream.nil)
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
-- = Experimental APIs
--
-- See "Streamly.Internal.System.Command" for unreleased APIs.
--

module Streamly.System.Command
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Exceptions
      Process.ProcessFailure (..)

    -- * Process Configuration
    -- | Use the config modifiers to modify the default config.
    , Process.Config

    -- ** Common Modifiers
    -- | These options apply to both POSIX and Windows.
    , Process.setCwd
    , Process.setEnv
    , Process.closeFiles
    , Process.newProcessGroup
    , Process.Session (..)
    , Process.setSession

    -- ** Posix Only Modifiers
    -- | These options have no effect on Windows.
    , Process.interruptChildOnly
    , Process.setUserId
    , Process.setGroupId

    -- ** Windows Only Modifiers
    -- | These options have no effect on Posix.
    , Process.waitForDescendants

    -- * Generation
    , toBytes
    , toChunks
    , toChunksWith
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
    , pipeChars

    -- -- * Including Stderr Stream
    -- | Like other "Generation" routines but along with stdout, stderr is also
    -- included in the output stream. stdout is converted to 'Right' values in
    -- the output stream and stderr is converted to 'Left' values.
    -- , toBytesEither
    -- , toChunksEither
    -- , toChunksEitherWith
    -- , pipeBytesEither
    -- , pipeChunksEither
    -- , pipeChunksEitherWith

    -- * Non-streaming Processes
    -- | These processes do not attach the IO streams with other processes.
    , foreground
    , daemon
    , standalone

    -- -- * Helpers
    -- , runWith
    -- , streamWith
    -- , pipeWith
    )
where

import Streamly.Internal.System.Command
import qualified Streamly.Internal.System.Process as Process -- (ProcessFailure (..))

-- Keep it synced with the Internal module

#include "DocTestCommand.hs"
-- Note: Commands are not executed using shell
--
-- You can use this module to execute system commands and compose them with
-- Haskell.  It does not use the system shell to execute commands, they are
-- executed as independent processes. This provides a convenient and powerful
-- way to replace shell scripting with Haskell. Instead of composing commands
-- using shell you can use Haskell Streamly streaming APIs to compose them with
-- better efficiency and type safety.
--
-- Normally, you should not need the system shell but if you want to use shell
-- scripts in your program then you can take a look at the @streamly-shell@
-- package which provides convenient wrapper over "Streamly.System.Process" to
-- execute shell scripts, commands.
