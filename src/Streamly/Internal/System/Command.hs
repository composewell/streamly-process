-- |
-- Module      : Streamly.Internal.System.Command
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Streamly.Internal.System.Command
    (
    -- * Generation
      toBytes
    , toChunks
    , toChunksWith
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
    , pipeChunksWith

    -- * Standalone Processes
    , standalone
    , foreground
    , daemon

    -- * Low-level Functions
    , shellWord
    , runWith
    , streamWith
    , pipeWith

    -- * Deprecated
    , quotedWord
    )
where

import Control.Exception (displayException)
import Control.Monad.Catch (MonadCatch, throwM)
import Data.Char (isSpace)
import GHC.IO.Exception (IOException(..), IOErrorType(..))
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Parser (Parser)
import Streamly.Data.Stream.Prelude (MonadAsync, Stream)
import Streamly.Internal.System.Process (Config)
import System.Exit (ExitCode(..))
import System.Process (ProcessHandle)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.System.Process as Process

-- Keep it synced with the released module

#include "DocTestCommand.hs"

-- | Parse one word of a command string, processing quotes and escapes as
-- in a POSIX shell:
--
-- * Words are separated by whitespace.
-- * Outside quotes, a backslash is removed and the next character is taken
--   literally, e.g. @a\\ b@ is the single word @a b@.
-- * Inside single quotes all characters are literal, including the
--   backslash.
-- * Inside double quotes, a backslash before @$@, @`@, @"@ or @\\@ is
--   removed; before any other character the backslash is kept.
-- * Quoted and unquoted parts with no whitespace between them form one word,
--   e.g. @'a'"b"c@ is the word @abc@.
--
-- Examples (using shell @echo@ command):
--
-- $ echo 'hello\\"world'
-- hello\\"world
--
-- $ echo "hello\"\\w\'orld"
-- hello"\w\'orld
--
-- $ echo 'hello\'
-- hello\
--
-- The parser fails if a quote is not terminated or if the input ends with a
-- backslash.
--
-- Differences from a POSIX shell:
--
-- * No expansion is performed: @$VAR@, @~@, globs and command substitution
--   are passed literally.
-- * Operators such as @|@, @;@, @&&@, @>@ and @<@ are ordinary characters.
-- * A backslash followed by a newline is not removed. Outside quotes the
--   newline is kept, inside double quotes both characters are kept.
-- * When used with @parseMany@, an input consisting only of whitespace
--   produces one empty word.
{-# INLINE shellWord #-}
shellWord :: MonadCatch m => Parser Char m String
shellWord =
    let toRQuote x =
            case x of
                '"' -> Just x
                '\'' -> Just x
                _ -> Nothing
        -- Inside double quotes, the backslash is removed before these
        -- characters.
        trEsc '"' x =
            case x of
                '\\' -> Just '\\'
                '"' -> Just '"'
                '$' -> Just '$'
                '`' -> Just '`'
                _ -> Nothing
        trEsc _ _ = Nothing
     in Parser.wordWithQuotes False trEsc '\\' toRQuote isSpace Fold.toList

{-# DEPRECATED quotedWord "Use shellWord instead." #-}
{-# INLINE quotedWord #-}
quotedWord :: MonadCatch m => Parser Char m String
quotedWord = shellWord

-- | An 'IOException' of type 'InvalidArgument' for an invalid command
-- string.
invalidCommand :: String -> String -> String -> IOException
invalidCommand fname reason cmd =
    IOError
        { ioe_handle = Nothing
        , ioe_type = InvalidArgument
        , ioe_location = fname
        , ioe_description = "command " ++ show cmd ++ ": " ++ reason
        , ioe_errno = Nothing
        , ioe_filename = Nothing
        }

-- | Split a command string into the executable and its arguments using
-- 'shellWord'. Raise an 'IOException' of type 'InvalidArgument' if the
-- command is empty or cannot be parsed. The first argument is the name of
-- the calling function, used in the exception.
{-# INLINE parseCommand #-}
parseCommand :: MonadCatch m => String -> String -> m (FilePath, [String])
parseCommand fname cmd = do
    xs <-
        Stream.fold Fold.toList
            $ Stream.mapM (either parseFailed return)
            $ Stream.parseMany shellWord
            $ Stream.fromList cmd
    case xs of
        -- shellWord returns one empty word for an all-whitespace string.
        y:ys | not (all isSpace cmd) -> return (y, ys)
        _ -> throwM $ invalidCommand fname "empty command" cmd

    where

    parseFailed e = throwM $ invalidCommand fname (displayException e) cmd

-- | A modifier for stream generation APIs in "Streamly.System.Process" to
-- generate streams from command strings.
--
-- For example:
--
-- >>> streamWith Process.toBytes "echo hello" & Stdio.putBytes
-- hello
-- >>> streamWith Process.toChunks "echo hello" & Stdio.putChunks
-- hello
--
-- An 'IOException' of type 'InvalidArgument' is raised if the command string
-- is empty or cannot be parsed by 'shellWord', e.g. it has an unterminated
-- quote.
--
-- /Internal/
{-# INLINE streamWith #-}
streamWith :: MonadCatch m =>
    (FilePath -> [String] -> Stream m a) -> String -> Stream m a
streamWith f cmd =
    Stream.concatEffect $ do
        (y, ys) <- parseCommand "streamWith" cmd
        return $ f y ys

-- | A modifier for process running APIs in "Streamly.System.Process" to run
-- command strings.
--
-- For example:
--
-- >>> runWith Process.toString "echo hello"
-- "hello\n"
-- >>> runWith Process.toStdout "echo hello"
-- hello
--
-- An 'IOException' of type 'InvalidArgument' is raised if the command string
-- is empty or cannot be parsed by 'shellWord', e.g. it has an unterminated
-- quote.
--
-- /Internal/
{-# INLINE runWith #-}
runWith :: MonadCatch m =>
    (FilePath -> [String] -> m a) -> String -> m a
runWith f cmd = do
    (y, ys) <- parseCommand "runWith" cmd
    f y ys

-- | A modifier for process piping APIs in "Streamly.System.Process" to pipe
-- data through processes specified by command strings.
--
-- For example:
--
-- >>> :{
--    toChunks "echo hello"
--  & pipeWith Process.pipeChunks "tr [a-z] [A-Z]"
--  & Stdio.putChunks
--  :}
--HELLO
--
-- An 'IOException' of type 'InvalidArgument' is raised if the command string
-- is empty or cannot be parsed by 'shellWord', e.g. it has an unterminated
-- quote.
--
-- /Internal/
pipeWith :: MonadCatch m =>
       (FilePath -> [String] -> Stream m a -> Stream m b)
    -> String
    -> Stream m a
    -> Stream m b
pipeWith f cmd input =
    Stream.concatEffect $ do
        (y, ys) <- parseCommand "pipeWith" cmd
        return $ f y ys input

-- | Like 'pipeChunks' but use the specified configuration to run the process.
{-# INLINE pipeChunksWith #-}
pipeChunksWith ::
    (MonadCatch m, MonadAsync m)
    => (Config -> Config)      -- ^ Config modifier
    -> String                  -- ^ Command
    -> Stream m (Array Word8)  -- ^ Input stream
    -> Stream m (Array Word8)  -- ^ Output stream
pipeChunksWith modifier = pipeWith (Process.pipeChunksWith modifier)

-- | @pipeChunks command input@ runs the executable with arguments specified by
-- @command@ and supplying @input@ stream as its standard input.  Returns the
-- standard output of the executable as a stream of byte arrays.
--
-- If only the name of an executable file is specified instead of its path then
-- the file name is searched in the directories specified by the PATH
-- environment variable.
--
-- If the input stream throws an exception or if the output stream is garbage
-- collected before it could finish then the process is terminated with SIGTERM.
--
-- If the process terminates with a non-zero exit code then a 'Process.ProcessFailure'
-- exception is raised.
--
-- If the process exits or closes its stdin before reading all of the input,
-- the rest of the input stream is not consumed and no exception is raised for
-- the failed write. Like in a shell pipeline, the result is determined by the
-- exit code of the process.
--
-- The following code is equivalent to the shell command @echo "hello world" |
-- tr [a-z] [A-Z]@:
--
-- >>> :{
--    toChunks "echo hello world"
--  & pipeChunks "tr [a-z] [A-Z]"
--  & Stdio.putChunks
--  :}
--HELLO WORLD
--
-- /Pre-release/
{-# INLINE pipeChunks #-}
pipeChunks :: (MonadAsync m, MonadCatch m) =>
    String -> Stream m (Array Word8) -> Stream m (Array Word8)
pipeChunks = pipeWith Process.pipeChunks

-- | Like 'pipeChunks' except that it works on a stream of bytes instead of
-- a stream of chunks.
--
-- >>> :{
--    toBytes "echo hello world"
--  & pipeBytes "tr [a-z] [A-Z]"
--  & Stdio.putBytes
--  :}
--HELLO WORLD
--
-- /Pre-release/
{-# INLINE pipeBytes #-}
pipeBytes :: (MonadAsync m, MonadCatch m) =>
    String -> Stream m Word8 -> Stream m Word8
pipeBytes = pipeWith Process.pipeBytes

-- | Like 'pipeChunks' except that it works on a stream of chars instead of
-- a stream of chunks.
--
-- >>> :{
--    toChars "echo hello world"
--  & pipeChars "tr [a-z] [A-Z]"
--  & Stdio.putChars
--  :}
--HELLO WORLD
--
-- /Pre-release/
{-# INLINE pipeChars #-}
pipeChars :: (MonadAsync m, MonadCatch m) =>
    String -> Stream m Char -> Stream m Char
pipeChars = pipeWith Process.pipeChars

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-- >>> toBytes = streamWith Process.toBytes

-- |
-- >>> toBytes "echo hello world" & Stdio.putBytes
--hello world
-- >>> toBytes "echo hello\\ world" & Stdio.putBytes
--hello world
-- >>> toBytes "echo 'hello world'" & Stdio.putBytes
--hello world
-- >>> toBytes "echo \"hello world\"" & Stdio.putBytes
--hello world
--
-- /Pre-release/
{-# INLINE toBytes #-}
toBytes :: (MonadAsync m, MonadCatch m) => String -> Stream m Word8
toBytes = streamWith Process.toBytes

-- | Like 'toChunks' but use the specified configuration to run the process.
{-# INLINE toChunksWith #-}
toChunksWith ::
    (MonadCatch m, MonadAsync m)
    => (Config -> Config)     -- ^ Config modifier
    -> String                 -- ^ Command
    -> Stream m (Array Word8) -- ^ Output stream
toChunksWith modifier = streamWith (Process.toChunksWith modifier)

-- >>> toChunks = streamWith Process.toChunks

-- |
-- >>> toChunks "echo hello world" & Stdio.putChunks
--hello world
--
-- /Pre-release/
{-# INLINE toChunks #-}
toChunks :: (MonadAsync m, MonadCatch m) => String -> Stream m (Array Word8)
toChunks = streamWith Process.toChunks

-- >>> toChars = streamWith Process.toChars

-- |
-- >>> toChars "echo hello world" & Stdio.putChars
--hello world
--
-- /Pre-release/
{-# INLINE toChars #-}
toChars :: (MonadAsync m, MonadCatch m) => String -> Stream m Char
toChars = streamWith Process.toChars

-- >>> toLines f = streamWith (Process.toLines f)

-- |
-- >>> toLines Fold.toList "echo -e hello\\\\nworld" & Stream.fold Fold.toList
-- ["hello","world"]
--
-- /Pre-release/
{-# INLINE toLines #-}
toLines ::
    (MonadAsync m, MonadCatch m)
    => Fold m Char a
    -> String       -- ^ Command
    -> Stream m a -- ^ Output Stream
toLines f = streamWith (Process.toLines f)

-- >>> toString = runWith Process.toString

-- |
-- >>> toString "echo hello world"
--"hello world\n"
--
-- /Pre-release/
{-# INLINE toString #-}
toString ::
    (MonadAsync m, MonadCatch m)
    => String       -- ^ Command
    -> m String
toString = runWith Process.toString

-- >>> toStdout = runWith Process.toStdout

-- |
-- >>> toStdout "echo hello world"
-- hello world
--
-- /Pre-release/
{-# INLINE toStdout #-}
toStdout ::
    (MonadAsync m, MonadCatch m)
    => String       -- ^ Command
    -> m ()
toStdout = runWith Process.toStdout

-- >>> toNull = runWith Process.toNull

-- |
-- >>> toNull "echo hello world"
--
-- /Pre-release/
{-# INLINE toNull #-}
toNull ::
    (MonadAsync m, MonadCatch m)
    => String -- ^ Command
    -> m ()
toNull = runWith Process.toNull

-------------------------------------------------------------------------------
-- Processes not interacting with the parent process
-------------------------------------------------------------------------------

-- | Launch a standlone process i.e. the process does not have a way to attach
-- the IO streams with other processes. The IO streams stdin, stdout, stderr
-- can either be inherited from the parent or closed.
--
-- This API is more powerful than 'foreground' and 'daemon' and can be used to
-- implement both of these. However, it should be used carefully e.g. if you
-- inherit the IO streams and parent is not waiting for the child process to
-- finish then both parent and child may use the IO streams resulting in
-- garbled IO if both are reading/writing simultaneously.
--
-- If the parent chooses to wait for the process an 'ExitCode' is returned
-- otherwise a 'ProcessHandle' is returned which can be used to terminate the
-- process, send signals to it or wait for it to finish. If the parent does
-- not wait, it must call @waitForProcess@ on the 'ProcessHandle' to reap the
-- process after it exits, otherwise the process remains a zombie until the
-- parent exits.
{-# INLINE standalone #-}
standalone ::
       Bool -- ^ Wait for process to finish?
    -> (Bool, Bool, Bool) -- ^ close (stdin, stdout, stderr)
    -> (Config -> Config)
    -> String -- ^ Command
    -> IO (Either ExitCode ProcessHandle)
standalone wait streams modCfg =
    runWith (Process.standalone wait streams modCfg)

-- | Launch a process interfacing with the user. User interrupts are sent to
-- the launched process and ignored by the parent process. The launched process
-- inherits stdin, stdout, and stderr from the parent, so that the user can
-- interact with the process. The parent waits for the child process to exit,
-- an 'ExitCode' is returned when the process finishes.
--
-- This is the same as the common @system@ function found in other libraries
-- used to execute commands.
--
-- On Windows you can pass @setSession NewConsole@ to create a new console.
--
{-# INLINE foreground #-}
foreground ::
       (Config -> Config)
    -> String -- ^ Command
    -> IO ExitCode
foreground modCfg = runWith (Process.foreground modCfg)

-- | Launch a daemon process. Closes stdin, stdout and stderr, creates a new
-- session, detached from the terminal, the parent does not wait for the
-- process to finish.
--
-- The 'ProcessHandle' returned can be used to terminate the daemon or send
-- signals to it. Call @waitForProcess@ on it to reap the process after it
-- exits, otherwise the process remains a zombie until the parent exits.
--
{-# INLINE daemon #-}
daemon ::
       (Config -> Config)
    -> String -- ^ Command
    -> IO ProcessHandle
daemon modCfg = runWith (Process.daemon modCfg)
