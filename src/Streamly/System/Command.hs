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

    -- * Generation
      toBytes
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

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> import Data.Char (toUpper)
-- >>> import Data.Function ((&))
-- >>> import qualified Streamly.Internal.Console.Stdio as Stdio
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.System.Process as Process
-- >>> import qualified Streamly.Unicode.Stream as Unicode
-- >>> import qualified Streamly.Internal.Data.Stream as Stream

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
-- You can use the streamly 'Streamly.Unicode.String.str' quasiquoter to write
-- commands conveniently e.g.:
--
-- >>> [str|ls -al "file name with spaces"|]
--
-- You can also interpolate strings expanding Haskell variables:
--
-- >>> f = "file name"
-- >>> [str|ls -al "#{f} with spaces"|]
--
-- Now you can write the examples in the "Streamly.System.Process" module more
-- conveniently.
--
-- = Executables as functions
--
-- The shell command @echo "hello world" | tr [a-z] [A-Z]@ can be written as:
--
-- >>> :{
--    Process.toBytes [str|echo "hello world"|]
--  & Process.pipeBytes [str|tr [a-z] [A-Z]|]
--  & Stream.fold Stdio.write
--  :}
--  HELLO WORLD
--
-- = Shell commands as functions
--
-- >>> :{
--    Process.toBytes [str|sh "-c" "echo hello | tr [a-z] [A-Z]"|]
--  & Stream.fold Stdio.write
--  :}
--  HELLO
