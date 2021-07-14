-- |
-- Module      : Streamly.System.Process
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides functions to turn operating system processes into
-- stream source, sink or transformation functions. Thus OS processes can be
-- used like regular Haskell stream functions connecting them into a stream
-- pipeline consisting of Haskell functions or other OS processes.
--
-- Processes can be composed in a streaming pipeline just like a Posix shell
-- command pipeline except that we use @&@ instead of @|@. Also note that like
-- the @pipefail@ option in shells, exceptions are propagated if any of the
-- stages fail.
--
-- The default attributes of the new process created by the APIs in this module
-- are described below:
--
-- * The following attributes are inherited from the parent:
--
--     * Current working directory
--     * Environment
--     * Process group
--     * Process uid and gid
--     * Signal handlers
--     * Terminal (Session)
--
-- * All fds except stdin, stdout and stderr are closed in the child

-- TODO:
--
-- - Need a way to specify additional parameters for process creation.
-- Possibly use something like @processBytesWith spec@ etc.
--
-- - Need a way to access the pid and manage the processes and process groups.
-- We can treat the processes in the same way as we treat threads. We can
-- compose processes in parallel, and cleanup can happen in the same way as
-- tids are cleaned up. But do we need this when we have threads anyway?
--
-- - Use unfolds for generation?
--
-- - Folds for composing process sinks? Input may be taken as input of the
-- fold and the output of the process can be consumed by another fold.
--
-- - Replace FilePath with a typed path.
--
{-# LANGUAGE FlexibleContexts #-}

module Streamly.System.Process
    ( ProcessFailure (..)

    -- * Generation
    , toBytes
    , toBytes'
    , toChunks
    , toChunks'

    -- * Transformation
    , processBytes
    , processBytes'
    , processChunks
    , processChunks'
    )
where

import Control.Exception (Exception, displayException)
import Control.Monad.Catch (MonadCatch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either (isRight, fromRight)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Array.Foreign (Array)
import Streamly.Prelude (MonadAsync, parallel, IsStream, adapt)
import System.Exit (ExitCode(..))
import System.IO (hClose, Handle)
import System.Process
    ( ProcessHandle
    , CreateProcess(..)
    , StdStream (..)
    , createProcess
    , waitForProcess
    , CmdSpec(..)
    )

import qualified Streamly.Data.Array.Foreign as Array
import qualified Streamly.Prelude as Stream

-- Internal imports
import Streamly.Internal.Data.Array.Foreign.Type (defaultChunkSize)
import Streamly.Internal.Data.Stream.StreamD.Step (Step (..))
import Streamly.Internal.Data.Stream.StreamD.Type
    (Stream (..), fromStreamD, toStreamD)
import Streamly.Internal.Data.SVar (adaptState)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import qualified Streamly.Internal.Data.Array.Stream.Foreign
    as ArrayStream (arraysOf)
import qualified Streamly.Internal.FileSystem.Handle
    as Handle (toChunks, putChunks)

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> import Data.Function ((&))
-- >>> import qualified Streamly.Console.Stdio as Stdio
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.System.Process as Process
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream

-- XXX To be imported from streamly in future releases.

data UnfoldManyEither o i f =
      UnfoldManyEitherOuter o
    | UnfoldManyEitherInner o i f

{-# INLINE [1] unfoldManyEitherD #-}
unfoldManyEitherD :: Monad m =>
    Unfold m a b -> Stream m (Either a a) -> Stream m (Either b b)
unfoldManyEitherD (Unfold istep inject) (Stream ostep ost) =
    Stream step (UnfoldManyEitherOuter ost)
  where
    {-# INLINE [0] step #-}
    step gst (UnfoldManyEitherOuter o) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield (Left a) o' -> do
                i <- inject a
                i `seq` return (Skip (UnfoldManyEitherInner o' i Left))
            Yield (Right a) o' -> do
                i <- inject a
                i `seq` return (Skip (UnfoldManyEitherInner o' i Right))
            Skip o' -> return $ Skip (UnfoldManyEitherOuter o')
            Stop -> return Stop

    step _ (UnfoldManyEitherInner o i f) = do
        r <- istep i
        return $ case r of
            Yield x i' -> Yield (f x) (UnfoldManyEitherInner o i' f)
            Skip i'    -> Skip (UnfoldManyEitherInner o i' f)
            Stop       -> Skip (UnfoldManyEitherOuter o)

{-# INLINE unfoldManyEither #-}
unfoldManyEither ::(IsStream t, Monad m) =>
    Unfold m a b -> t m (Either a a) -> t m (Either b b)
unfoldManyEither u m = fromStreamD $ unfoldManyEitherD u (toStreamD m)

{-# INLINE rights #-}
rights :: (IsStream t, Monad m, Functor (t m)) => t m (Either a b) -> t m b
rights = fmap (fromRight undefined) . Stream.filter isRight

-- | Represents the failure exit code of a process.
--
-- @since 0.1.0
newtype ProcessFailure = ProcessFailure Int
    deriving Show

-- Exception instance of ProcessFailure
instance Exception ProcessFailure where

    displayException (ProcessFailure exitCode) =
        "Process failed with exit code: " ++ show exitCode

-- |
-- Takes a process handle and waits for the process to exit, and then
-- raises 'ProcessFailure' exception if process failed with some
-- exit code, else peforms no action
--
wait :: MonadIO m => ProcessHandle -> m ()
wait procHandle = liftIO $ do
    exitCode <- waitForProcess procHandle
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure exitCodeInt -> throwM $ ProcessFailure exitCodeInt

-- Set everything explicitly so that we are immune to changes upstream.
procAttrs :: FilePath -> [String] -> CreateProcess
procAttrs path args = CreateProcess
    { cmdspec = RawCommand path args
    , cwd = Nothing -- inherit
    , env = Nothing -- inherit

    -- File descriptors
    , std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = Inherit
    , close_fds = True  -- False?

    -- Session/group/setuid/setgid
    , create_group = False
    , child_user = Nothing  -- Posix only
    , child_group = Nothing  -- Posix only

    -- Signals (Posix only) behavior
    -- Reset SIGINT (Ctrl-C) and SIGQUIT (Ctrl-\) to default handlers.
    , delegate_ctlc = False

    -- Terminal behavior
    , new_session = False  -- Posix only
    , detach_console = False -- Windows only
    , create_new_console = False -- Windows Only

    -- Added by commit 6b8ffe2ec3d115df9ccc047599545ca55c393005
    , use_process_jobs = True -- Windows only
    }

pipeStdErr :: CreateProcess -> CreateProcess
pipeStdErr cfg = cfg { std_err = CreatePipe }

-- | Creates a system process from an executable path and arguments. For the
-- default attributes used to create the process see 'procAttrs'.
--
createProc' ::
       (CreateProcess -> CreateProcess) -- ^ Process attribute modifier
    -> FilePath                         -- ^ Executable path
    -> [String]                         -- ^ Arguments
    -> IO (Handle, Handle, Handle, ProcessHandle)
    -- ^ (Input Handle, Output Handle, Error Handle, Process Handle)
createProc' modCfg path args = do
    (Just stdinH, Just stdoutH, Just stderrH, procH)
        <- createProcess $ modCfg $ procAttrs path args
    return (stdinH, stdoutH, stderrH, procH)

{-# INLINE processChunks' #-}
processChunks' ::
    (IsStream t, MonadCatch m, MonadAsync m)
    => FilePath             -- ^ Path to Executable
    -> [String]             -- ^ Arguments
    -> t m (Array Word8)    -- ^ Input stream
    -> t m (Either (Array Word8) (Array Word8))     -- ^ Output stream
processChunks' path args input = Stream.bracket alloc cleanup run

    where

    alloc = liftIO $ createProc' pipeStdErr path args

    cleanup (stdinH, stdoutH, stderrH, procH) = do
        liftIO $ hClose stdinH >> hClose stdoutH >> hClose stderrH
        wait procH

    runStdin stdinH =
        Stream.before
            (Handle.putChunks stdinH (adapt input) >> liftIO (hClose stdinH))
            Stream.nil

    runStdout stdoutH =
        Stream.after
            (liftIO $ hClose stdoutH)
            (Stream.map Right (Handle.toChunks stdoutH))

    runStderr stderrH =
        Stream.after
            (liftIO $ hClose stderrH)
            (Stream.map Left (Handle.toChunks stderrH))

    run (stdinH, stdoutH, stderrH, _) =
        runStdin stdinH
            `parallel` runStderr stderrH
            `parallel` runStdout stdoutH

-- | @processBytes' path args input@ runs the executable at @path@ using @args@
-- as arguments and @input@ stream as its standard input.  The error stream of
-- the executable is presented as 'Left' values in the resulting stream and
-- output stream as 'Right' values.
--
-- Raises 'ProcessFailure' exception in case of failure.
--
-- For example, the following is equivalent to the shell command @echo "hello
-- world" | tr [:lower:] [:upper:]@:
--
-- >>> :{
--    Process.processBytes' "/bin/echo" ["hello world"] Stream.nil
--  & Stream.rights
--  & Process.processBytes' "/bin/tr" ["[:lower:]", "[:upper:]"]
--  & Stream.rights
--  & Stream.fold Stdio.write
--  :}
--HELLO WORLD
--
-- @since 0.1.0
{-# INLINE processBytes' #-}
processBytes' ::
    (IsStream t, MonadCatch m, MonadAsync m)
    => FilePath         -- ^ Executable path
    -> [String]         -- ^ Arguments
    -> t m Word8        -- ^ Input Stream
    -> t m (Either Word8 Word8) -- ^ Output Stream
processBytes' path args input =
    let input1 = ArrayStream.arraysOf defaultChunkSize input
        output = processChunks' path args input1
     in unfoldManyEither Array.read output

-- |
-- Runs a process specified by the path to executable, arguments
-- that are to be passed and input to be provided to the process
-- (standard input) and returns the output of the process (standard output).
--
-- Raises an exception 'ProcessFailure' if process failed due to some
-- reason
--
-- @since 0.1.0
{-# INLINE processBytes #-}
processBytes ::
    (IsStream t, MonadCatch m, MonadAsync m, Functor (t m))
    => FilePath     -- ^ Path to executable
    -> [String]     -- ^ Arguments to pass to executable
    -> t m Word8    -- ^ Input Stream
    -> t m Word8    -- ^ Output Stream
processBytes path args = rights . processBytes' path args

-- |
-- Runs a process specified by the path to executable, arguments
-- that are to be passed and input to be provided to the process
-- (standard input) in the form of a stream of array of Word8
-- and returns the output of the process (standard output) in
-- the form of a stream of array of Word8
--
-- Raises an exception 'ProcessFailure' If process failed due to some
-- reason
--
-- @since 0.1.0
{-# INLINE processChunks #-}
processChunks ::
    (IsStream t, MonadCatch m, MonadAsync m, Functor (t m))
    => FilePath             -- ^ Path to executable
    -> [String]             -- ^ Arguments to pass to executable
    -> t m (Array Word8)    -- ^ Input Stream
    -> t m (Array Word8)    -- ^ Output Stream
processChunks path args = rights . processChunks' path args

-- | @toBytes' path args@ runs the executable at @path@ using @args@ as
-- arguments and returns a stream of 'Either' bytes. The 'Left' values are from
-- @stderr@ and the 'Right' values are from @stdout@ of the executable.
--
-- Raises 'ProcessFailure' exception in case of failure.
--
-- The following example uses @echo@ to write @hello@ to @stdout@ and @world@
-- to @stderr@, then uses folds from "Streamly.Console.Stdio" to write them
-- back to @stdout@ and @stderr@ respectively:
--
--
-- >>> :{
--   Process.toBytes' "/bin/bash" ["-c", "echo 'hello'; echo 'world' 1>&2"]
-- & Stream.fold (Fold.partition Stdio.writeErr Stdio.write)
-- :}
-- world
-- hello
-- ((),())
--
-- >>> toBytes' path args = Process.processBytes' path args Stream.nil
--
-- @since 0.1.0
{-# INLINE toBytes' #-}
toBytes' ::
    (IsStream t, MonadAsync m, MonadCatch m)
    => FilePath     -- ^ Executable path
    -> [String]     -- ^ Arguments
    -> t m (Either Word8 Word8)    -- ^ Output Stream
toBytes' path args = processBytes' path args Stream.nil

-- | Like 'toBytes' but ignores the @stderr@ stream.
--
-- >>> toBytes path args = toBytes' path args & Stream.rights
--
{-# INLINE toBytes #-}
toBytes ::
    (IsStream t, MonadAsync m, MonadCatch m, Functor (t m))
    => FilePath     -- ^ Executable path
    -> [String]     -- ^ Arguments
    -> t m Word8    -- ^ Output Stream
toBytes path args = toBytes' path args & rights

-- | Like 'toBytes' but generates a stream of @Array Word8@ instead of a stream
-- of @Word8@.
--
-- >>> :{
--   Process.toChunks' "/bin/bash" ["-c", "echo 'hello'; echo 'world' 1>&2"]
-- & Stream.fold (Fold.partition Stdio.writeErrChunks Stdio.writeChunks)
-- :}
-- world
-- hello
-- ((),())
--
-- >>> toChunks' path args = Process.processChunks' path args Stream.nil
--
-- Prefer 'toChunks' over 'toBytes' when performance matters.
--
-- @since 0.1.0
{-# INLINE toChunks' #-}
toChunks' ::
    (IsStream t, MonadAsync m, MonadCatch m)
    => FilePath             -- ^ Executable path
    -> [String]             -- ^ Arguments
    -> t m (Either (Array Word8) (Array Word8))    -- ^ Output Stream
toChunks' path args = processChunks' path args Stream.nil

-- | Like 'toChunks' but ignores the @stderr@ stream.
--
-- >>> toChunks path args = toChunks' path args & Stream.rights
--
{-# INLINE toChunks #-}
toChunks ::
    (IsStream t, MonadAsync m, MonadCatch m, Functor (t m))
    => FilePath             -- ^ Executable path
    -> [String]             -- ^ Arguments
    -> t m (Array Word8)    -- ^ Output Stream
toChunks path args = toChunks' path args & rights
