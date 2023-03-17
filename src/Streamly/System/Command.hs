-- |
-- Module      : Streamly.System.Command
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Use command strings to execute OS processes and use them just like native
-- Haskell functions to generate, transform or consume streams. It provides a
-- powerful way to write high level Haskell scripts to perform tasks like shell
-- scripts but not requiring the shell and with C-like performance.
--
-- This module is a higher level wrapper over the "Streamly.System.Process"
-- module.
--
-- See also: "Streamly.Internal.System.Command".
--
module Streamly.System.Command
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Overview
    -- $overview

    -- * Types
      ProcessFailure (..)

    -- * Generation
    , toBytes
    , toChunks
    , toChars
    , toLines

    -- * Effects
    , toString
    , toStdout
    , toNull

    -- * Transformation
    , pipeBytes
    , pipeChars
    , pipeChunks

    -- -- * Helpers
    -- , runWith
    -- , streamWith
    -- , pipeWith
    )
where

import Streamly.Internal.System.Command
import Streamly.Internal.System.Process (ProcessFailure (..))

-- Keep it synced with the Internal module

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> :set -XQuasiQuotes
-- >>> import Data.Char (toUpper)
-- >>> import Data.Function ((&))
-- >>> import Streamly.Unicode.String (str)
-- >>> import qualified Streamly.Data.Array as Array
-- >>> import qualified Streamly.Console.Stdio as Stdio
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Stream.Prelude as Stream
-- >>> import qualified Streamly.System.Command as Command
-- >>> import qualified Streamly.Unicode.Stream as Unicode
--
-- >>> import qualified Streamly.Internal.System.Process as Process
-- >>> import qualified Streamly.Internal.Console.Stdio as Stdio
-- >>> import qualified Streamly.Internal.FileSystem.Dir as Dir

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

-- $overview
--
-- Please see the "Streamly.System.Process" for basics.
--
-- "Streamly.System.Process" module requires specifying the command executable
-- name and its arguments separately (e.g. "ls" "-al") whereas using this
-- module we can specify the executable and its arguments more conveniently as
-- a single command string e.g.  we can execute "ls -al".
--
-- A command string is parsed in the same way as a posix shell would parse it.
-- A command string consists of whitespace separated tokens with the first
-- token treated as the executable name and the rest as arguments. Whitespace
-- can be escaped using @\@. Alternatively, double quotes or single quotes can
-- be used to enclose tokens with whitespaces. Quotes can be escaped using @\@.
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
-- Now you can write the examples in the "Streamly.System.Process" module more
-- conveniently.
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
-- If you want to execute the same command using the shell:
--
-- >>> :{
--    Command.toBytes [str|sh "-c" "echo 'hello world' | tr [a-z] [A-Z]"|]
--  & Stream.fold Stdio.write
--  :}
--  HELLO WORLD
--
-- = Running Commands Concurrently
--
-- Running @grep@ concurrently on many files:
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
