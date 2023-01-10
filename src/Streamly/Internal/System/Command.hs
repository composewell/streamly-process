-- |
-- Module      : Streamly.Internal.System.Command
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This is a higher level convenience wrapper module over the lower level
-- module "Streamly.System.Process". The Process module requires specifying the
-- command executable and its arguments separately (e.g. "ls" "-al") whereas
-- using this module we can specify the executable and its arguments as a
-- single command string e.g.  we can execute "ls -al". The command string is
-- parsed in the same way as the posix shell would parse it, quotes must be
-- balanced unless escaped. Quoted strings are treated as a single token,
-- however, quotes are not stripped and passed verbatim to the process (We
-- should perhaps eat the quotes?).  Backslash can also be used to escape
-- space.
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

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Streamly.Internal.System.Command
    (
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

    -- * Helpers
    , runWith
    , streamWith
    , pipeWith
    )
where

import Control.Monad.Catch (MonadCatch)
import Data.Char (isSpace)
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Stream.Prelude (MonadAsync, Stream)
import Streamly.Internal.Data.Parser (Parser)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.System.Process as Process

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> import Data.Char (toUpper)
-- >>> import Data.Function ((&))
-- >>> import qualified Streamly.Internal.Console.Stdio as Stdio
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.System.Process as Process
-- >>> import qualified Streamly.Unicode.Stream as Unicode
-- >>> import qualified Streamly.Internal.Data.Stream as Stream

{-# INLINE quotedWord #-}
quotedWord :: MonadCatch m => Parser Char m String
quotedWord =
    let isQt = (`elem` ['"', '\''])
     in Parser.wordQuotedBy False (== '\\') isQt isQt id isSpace Fold.toList

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
-- /Internal/
{-# INLINE streamWith #-}
streamWith :: MonadCatch m =>
    (FilePath -> [String] -> Stream m a) -> String -> Stream m a
streamWith f cmd =
    Stream.concatMapM (\() -> do
        xs <- Stream.fold Fold.toList
                $ Stream.catRights
                $ Stream.parseMany quotedWord
                $ Stream.fromList cmd
        case xs of
            y:ys -> return $ f y ys
            _ -> error "streamWith: empty command") (Stream.fromPure ())

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
-- /Internal/
{-# INLINE runWith #-}
runWith :: MonadCatch m =>
    (FilePath -> [String] -> m a) -> String -> m a
runWith f cmd = do
    xs <- Stream.fold Fold.toList
            $ Stream.catRights
            $ Stream.parseMany quotedWord
            $ Stream.fromList cmd
    case xs of
        y:ys -> f y ys
        _ -> error "streamWith: empty command"

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
-- /Internal/
pipeWith :: MonadCatch m =>
       (FilePath -> [String] -> Stream m a -> Stream m b)
    -> String
    -> Stream m a
    -> Stream m b
pipeWith f cmd input =
    Stream.concatMapM (\() -> do
        xs <- Stream.fold Fold.toList
                $ Stream.catRights
                $ Stream.parseMany quotedWord
                $ Stream.fromList cmd
        case xs of
            y:ys -> return $ f y ys input
            _ -> error "streamWith: empty command") (Stream.fromPure ())


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
-- If the process terminates with a non-zero exit code then a 'ProcessFailure'
-- exception is raised.
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

-- |
--
-- >>> toBytes = streamWith Process.toBytes
--
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

-- |
--
-- >>> toChunks = streamWith Process.toChunks
--
-- >>> toChunks "echo hello world" & Stdio.putChunks
--hello world
--
-- /Pre-release/
{-# INLINE toChunks #-}
toChunks :: (MonadAsync m, MonadCatch m) => String -> Stream m (Array Word8)
toChunks = streamWith Process.toChunks

-- |
-- >>> toChars = streamWith Process.toChars
--
-- >>> toChars "echo hello world" & Stdio.putChars
--hello world
--
-- /Pre-release/
{-# INLINE toChars #-}
toChars :: (MonadAsync m, MonadCatch m) => String -> Stream m Char
toChars = streamWith Process.toChars

-- |
-- >>> toLines f = streamWith (Process.toLines f)
--
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

-- |
-- >>> toString = runWith Process.toString
--
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

-- |
-- >>> toStdout = runWith Process.toStdout
--
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

-- |
-- >>> toNull = runWith Process.toNull
--
-- >>> toNull "echo hello world"
--
-- /Pre-release/
{-# INLINE toNull #-}
toNull ::
    (MonadAsync m, MonadCatch m)
    => String -- ^ Command
    -> m ()
toNull = runWith Process.toNull
