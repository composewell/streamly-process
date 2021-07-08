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

-- TODO:
--
-- 1) Provide a function "cmd :: String -> (FilePath, [String])" and change the
-- signatures to something like "toBytes :: (FilePath, [String]) -> ...", so
-- that we can use something like @toBytes (cmd "ls -al")@.
--
-- 2) Need a way to specify additional parameters for process creation.
-- Possibly use something like @processBytesWith spec@ etc. Another way is to
-- use a StateT like environment (shell environment).
--
-- 3) Need a way to access the pid and manage the processes and process groups.
-- We can treat the processes in the same way as we treat threads. We can
-- compose processes in parallel, and cleanup can happen in the same way as
-- tids are cleaned up.
--
-- 4) Use unfolds for generation?
--
-- 5) Folds for composing process sinks? Input may be taken as input of the
-- fold and the output of the process can be consumed by another fold.
--
-- 6) Replace FilePath with a typed path.
--
-- 7) iterateBytes API that feeds back the error to the input of the process.
--
{-# LANGUAGE FlexibleContexts #-}

module Streamly.System.Process
    ( ProcessFailure (..)

    -- * Generation
    , toBytes
    , toChunks

    -- * Transformation
    , processBytes
    , processBytes_
    , processChunks
    , processChunks_
    )
where

import Control.Exception (Exception, displayException)
import Control.Monad.Catch (MonadCatch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word8)
import Streamly.Data.Array.Foreign (Array)
import Streamly.Data.Fold (Fold)
import Streamly.Prelude (MonadAsync, parallel, IsStream, adapt)
import System.Exit (ExitCode(..))
import System.IO (hClose, Handle)
import System.Process
    ( ProcessHandle
    , CreateProcess(..)
    , StdStream (..)
    , createProcess
    , proc
    , waitForProcess
    )

import qualified Streamly.Prelude as Stream

-- Internal imports
import qualified Streamly.Internal.Data.Array.Stream.Foreign
    as ArrayStream (concat)
import qualified Streamly.Internal.FileSystem.Handle
    as Handle (toBytes, toChunks, putBytes)

-- $setup
-- >>> import qualified Streamly.Console.Stdio as Stdio
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.System.Process as Process

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

-- |
-- Creates a process using the path to executable and arguments, then
-- connects a pipe's write end with output of the process, and
-- returns the read end's handle and the process handle of the process
--
{-# INLINE openProc #-}
openProc ::
    FilePath                        -- ^ Path to Executable
    -> [String]                     -- ^ Arguments
    -> IO (Handle, ProcessHandle)
    -- ^ Handle to read from output of process, process handle
openProc fpath args = do
    let procObj = (proc fpath args) {
            std_out = CreatePipe,
            close_fds = True,
            use_process_jobs = True
        }

    (_, Just readEnd, _, procHandle) <- createProcess procObj
    return (readEnd, procHandle)

-- |
-- Creates a process using the path to executable and arguments, then
-- connects a pipe's write end with output of the process and
-- generates a stream based on a function which takes the read end of the
-- pipe and generates a stream
--
-- Raises an exception 'ProcessFailure' if process failed due to some
-- reason
--
{-# INLINE withExe #-}
withExe ::
        (IsStream t, MonadAsync m, MonadCatch m)
        => FilePath          -- ^ Path to Executable
        -> [String]          -- ^ Arguments
        -> (Handle -> t m a)
        -- ^ Given handle to read from output of
        -- process generate stream output
        -> t m a             -- ^ Stream produced
withExe fpath args genStrm = Stream.bracket pre post body

    where

    pre = liftIO $ openProc fpath args

    post (readHdl, procHandle) =
        liftIO $ hClose readHdl >> wait procHandle

    body (readHdl, _) = genStrm readHdl

-- |
-- Creates a process using the path to executable and arguments, then
-- builds two pipes, one whose read end is made the process's standard
-- input, and another whose write end is made the process's standard
-- output. The function returns the handle to write end to the
-- process's input, handle to read end to process's output and
-- process handle of the process
--
{-# INLINE openProcInp #-}
openProcInp ::
    FilePath                                -- ^ Path to Executable
    -> [String]                             -- ^ Arguments
    -> IO (Handle, Handle, ProcessHandle)
    -- ^ (Input Handle, Output Handle, Process Handle)
openProcInp fpath args = do
    let procObj = (proc fpath args) {
            std_in = CreatePipe,
            std_out = CreatePipe,
            close_fds = True,
            use_process_jobs = True
        }

    (Just writeInpEnd, Just readOutEnd, _, procHandle) <- createProcess procObj
    return (writeInpEnd, readOutEnd, procHandle)

-- |
-- Creates a process using the path to executable, arguments and a stream
-- which would be used as input to the process (passed as standard input),
-- then connects a pipe's write end with output of the process and generates
-- a stream based on a function which takes the read end of the pipe and
-- generates a stream.
--
-- Raises an exception 'ProcessFailure' if process failed due to some
-- reason
--
{-# INLINE withInpExe #-}
withInpExe ::
    (IsStream t, MonadAsync m, MonadCatch m)
    => FilePath             -- ^ Path to Executable
    -> [String]             -- ^ Arguments
    -> t m Word8            -- ^ Input stream to the process
    -> (Handle -> t m a)
    -- ^ Handle to read output of process and generate stream
    -> t m a                -- ^ Stream produced
withInpExe fpath args input genStrm = Stream.bracket pre post body

    where

    writeAction writeHdl =
        Handle.putBytes writeHdl (adapt input) >> liftIO (hClose writeHdl)

    pre = liftIO $ openProcInp fpath args

    post (_, readHdl, procHandle) =
        liftIO $ hClose readHdl >> wait procHandle

    body (writeHdl, readHdl, _) =
        parallel
            (Stream.before (writeAction writeHdl) Stream.nil)
            (genStrm readHdl)

-- | @toBytes path args@ runs the executable at @path@ using @args@ as
-- arguments and returns the output (@stdout@) of the executable as a stream of
-- bytes. The error stream (@stderr@) is ignored.
--
-- Raises 'ProcessFailure' exception in case of failure.
--
-- For example, the following is equivalent to the shell command @echo "hello
-- world" | cat@:
--
-- >>> Process.toBytes "/bin/echo" ["hello", "world"] & Stream.fold Stdio.write
-- hello world
--
-- @since 0.1.0
{-# INLINE toBytes #-}
toBytes ::
    (IsStream t, MonadAsync m, MonadCatch m)
    => FilePath     -- ^ Executable path
    -> [String]     -- ^ Arguments
    -> t m Word8    -- ^ Output Stream
toBytes fpath args = ArrayStream.concat $ withExe fpath args Handle.toChunks

-- | Like 'toBytes' but generates a stream of @Array Word8@ instead of a stream
-- of @Word8@.
--
-- For example, the following is equivalent to the shell command @echo "hello
-- world" | cat@:
--
-- >>> Process.toChunks "/bin/echo" ["hello", "world"] & Stream.fold Stdio.writeChunks
-- hello world
--
-- @since 0.1.0
toChunks ::
    (IsStream t, MonadAsync m, MonadCatch m)
    => FilePath             -- ^ Executable path
    -> [String]             -- ^ Arguments
    -> t m (Array Word8)    -- ^ Output Stream
toChunks fpath args = withExe fpath args Handle.toChunks

-- |
-- Runs a process specified by the path to executable, arguments
-- that are to be passed and input to be provided to the process
-- (standard input) and returns the output of the process (standard output).
--
-- Raises an exception 'ProcessFailure' if process failed due to some
-- reason
--
-- @since 0.1.0
{-# INLINE processBytes_ #-}
processBytes_ ::
    (IsStream t, MonadCatch m, MonadAsync m)
    => FilePath     -- ^ Path to executable
    -> [String]     -- ^ Arguments to pass to executable
    -> t m Word8    -- ^ Input Stream
    -> t m Word8    -- ^ Output Stream
processBytes_ fpath args inStream =
    ArrayStream.concat $ withInpExe fpath args inStream Handle.toChunks

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
{-# INLINE processChunks_ #-}
processChunks_ ::
    (IsStream t, MonadCatch m, MonadAsync m)
    => FilePath             -- ^ Path to executable
    -> [String]             -- ^ Arguments to pass to executable
    -> t m (Array Word8)    -- ^ Input Stream
    -> t m (Array Word8)    -- ^ Output Stream
processChunks_ fpath args inStream =
    withInpExe fpath args (ArrayStream.concat inStream) Handle.toChunks

-- |
-- Creates a process using the path to executable and arguments, then
-- builds three pipes, one whose read end is made the process's standard
-- input, and another whose write end is made the process's standard
-- output, and the final one's write end is made the process's standard error.
-- The function returns the handle to write end to the process's input, handle
-- to read end to process's output, handle to read end to process's standard
-- error and process handle of the process
--
openProcErr ::
    FilePath                                -- ^ Path to Executable
    -> [String]                             -- ^ Arguments
    -> IO (Handle, Handle, Handle, ProcessHandle)
    -- ^ (Input Handle, Output Handle, Error Handle, Process Handle)
openProcErr fpath args = do
    let spec = proc fpath args
        spec1 =
            spec
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                , close_fds = True
                , use_process_jobs = True
                }

    (Just stdinH, Just stdoutH, Just stderrH, procH)
        <- createProcess spec1
    return (stdinH, stdoutH, stderrH, procH)

-- |
-- Creates a process using the path to executable, arguments and a stream
-- which would be used as input to the process (passed as standard input),
-- then using a pipe, it reads from the process's standard error and folds
-- it using the supplied Fold, then connects another pipe's write end with
-- output of the process and generates a stream based on a function which
-- takes the read end of the pipe and generates a stream.
--
-- Raises an exception 'ProcessFailure' if process failed due to some
-- reason
--
withErrExe ::
    (IsStream t, MonadCatch m, MonadAsync m)
    => FilePath             -- ^ Path to Executable
    -> [String]             -- ^ Arguments
    -> Fold m Word8 b       -- ^ Fold to fold the error stream
    -> t m Word8            -- ^ Input stream to the process
    -> (Handle -> t m a)    -- ^ Output stream generator
    -> t m a                -- ^ Stream produced
withErrExe fpath args fld input fromHandle = Stream.bracket alloc cleanup run

    where

    alloc = liftIO $ openProcErr fpath args

    cleanup (stdinH, stdoutH, stderrH, procH) = do
        liftIO $ hClose stdinH >> hClose stdoutH >> hClose stderrH
        wait procH

    fromEffect_ eff = Stream.before eff Stream.nil

    runStderr stderrH = fromEffect_ $
        Stream.fold fld (Handle.toBytes stderrH)
            >> liftIO (hClose stderrH)

    runStdin stdinH = fromEffect_ $
        Handle.putBytes stdinH (adapt input)
            >> liftIO (hClose stdinH)

    runStdout stdoutH =
        Stream.after (liftIO $ hClose stdoutH) (fromHandle stdoutH)

    run (stdinH, stdoutH, stderrH, _) =
        runStdin stdinH
            `parallel` runStderr stderrH
            `parallel` runStdout stdoutH

-- | @processBytes path args errAccum input@ runs the executable at @path@
-- using @args@ as arguments and @input@ stream as its standard input.  The
-- error stream generated by the process is folded using the @errAccum@ 'Fold'.
-- The output (@stdout@) of the process is returned as a stream of bytes.
--
-- Raises 'ProcessFailure' exception in case of failure.
--
-- For example, the following is equivalent to the shell command @echo "hello
-- world" | tr [:lower:] [:upper:]@:
--
-- >>> :{
--    Process.toBytes "/bin/echo" ["hello world"]
--  & Process.processBytes "/bin/tr" ["[:lower:]", "[:upper:]"] Fold.drain
--  & Stream.fold Stdio.write
--  :}
-- HELLO WORLD
--
-- We can write 'toBytes' in terms of 'processBytes' by supplying a nil stream
-- as input:
--
-- >>> :{
--   Stream.nil
-- & processBytes "/bin/echo" ["hello"] Fold.drain
-- & Stream.fold Stdio.write
-- :}
-- hello
--
-- This is similar to a Posix shell pipeline except that we use @&@ instead of
-- @|@. Also note that like the @pipefail@ option in shells, exceptions are
-- propagated if any of the stages fail.
--
-- @since 0.1.0
{-# INLINE processBytes #-}
processBytes ::
    (IsStream t, MonadCatch m, MonadAsync m)
    => FilePath         -- ^ Executable path
    -> [String]         -- ^ Arguments
    -> Fold m Word8 b   -- ^ Error stream Fold
    -> t m Word8        -- ^ Input Stream
    -> t m Word8        -- ^ Output Stream
processBytes fpath args fld inStream =
    withErrExe fpath args fld inStream Handle.toBytes

-- |
-- Runs a process specified by the path to executable, arguments
-- that are to be passed and input to be provided to the process
-- (standard input) in the form of a array of Word8 and folds
-- the error stream using the given Fold along with returning
-- the output of the process (standard output) in the form of a
-- array of Word8
--
-- Raises an exception 'ProcessFailure' if process failed due to some
-- reason. The Fold would continue if you would catch the thrown exception.
--
-- @since 0.1.0
{-# INLINE processChunks #-}
processChunks ::
    (IsStream t, MonadCatch m, MonadAsync m)
    => FilePath                 -- ^ Path to executable
    -> [String]                 -- ^ Arguments to pass to executable
    -> Fold m Word8 b           -- ^ Fold to fold Error Stream
    -> t m (Array Word8)        -- ^ Input Stream
    -> t m (Array Word8)        -- ^ Output Stream
processChunks fpath args fld inStream =
    withErrExe fpath args fld (ArrayStream.concat inStream) Handle.toChunks
