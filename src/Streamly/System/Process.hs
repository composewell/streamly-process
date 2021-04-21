-- |
-- Module      : Streamly.System.Process
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

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

import Streamly.Data.Fold (Fold)
import Streamly.Memory.Array (Array)
import Streamly (MonadAsync, parallel, IsStream, adapt)
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.Memory.ArrayStream as AS

-- |
-- ProcessFailure exception which would be raised when process which
-- is made to run fails. The integer it contains is the exit code
-- of the failed process
--
-- @since 0.1.0.0
newtype ProcessFailure = ProcessFailure Int
    deriving Show

-- Exception instance of ProcessFailure
instance Exception ProcessFailure where

    displayException (ProcessFailure exitCodeInt) =
        "Process Failed With Exit Code " ++ show exitCodeInt

-- |
-- Takes a process handle and waits for the process to exit, and then
-- raises 'ProcessFailure' exception if process failed with some
-- exit code, else peforms no action
--
-- @since 0.1.0.0
exceptOnError :: MonadIO m => ProcessHandle -> m ()
exceptOnError procHandle = liftIO $ do
    exitCode <- waitForProcess procHandle
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure exitCodeInt -> throwM $ ProcessFailure exitCodeInt

-- |
-- Creates a process using the path to executable and arguments, then
-- connects a pipe's write end with output of the process, and
-- returns the read end's handle and the process handle of the process
--
-- @since 0.1.0.0
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
-- @since 0.1.0.0
{-# INLINE withExe #-}
withExe ::
        (IsStream t, MonadCatch m, MonadIO m)
        => FilePath          -- ^ Path to Executable
        -> [String]          -- ^ Arguments
        -> (Handle -> t m a)
        -- ^ Given handle to read from output of
        -- process generate stream output
        -> t m a             -- ^ Stream produced
withExe fpath args genStrm = S.bracket pre post body

    where

    pre = liftIO $ openProc fpath args

    post (readHdl, procHandle) =
        liftIO $ hClose readHdl >> exceptOnError procHandle

    body (readHdl, _) = genStrm readHdl

-- |
-- Creates a process using the path to executable and arguments, then
-- builds two pipes, one whose read end is made the process's standard
-- input, and another whose write end is made the process's standard
-- output. The function returns the handle to write end to the
-- process's input, handle to read end to process's output and
-- process handle of the process
--
-- @since 0.1.0.0
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
-- @since 0.1.0.0
{-# INLINE withInpExe #-}
withInpExe ::
    (IsStream t, MonadCatch m, MonadAsync m)
    => FilePath             -- ^ Path to Executable
    -> [String]             -- ^ Arguments
    -> t m Word8            -- ^ Input stream to the process
    -> (Handle -> t m a)
    -- ^ Handle to read output of process and generate stream
    -> t m a                -- ^ Stream produced
withInpExe fpath args input genStrm = S.bracket pre post body

    where

    writeAction writeHdl =
        FH.fromBytes writeHdl (adapt input) >> liftIO (hClose writeHdl)

    pre = liftIO $ openProcInp fpath args

    post (_, readHdl, procHandle) =
        liftIO $ hClose readHdl >> exceptOnError procHandle

    body (writeHdl, readHdl, _) =
        parallel (S.nilM $ writeAction writeHdl) (genStrm readHdl)

-- |
-- Creates a process using the path to executable and arguments, then
-- builds three pipes, one whose read end is made the process's standard
-- input, and another whose write end is made the process's standard
-- output, and the final one's write end is made the process's standard error.
-- The function returns the handle to write end to the process's input, handle
-- to read end to process's output, handle to read end to process's standard
-- error and process handle of the process
--
-- @since 0.1.0.0
openProcErr ::
    FilePath                                -- ^ Path to Executable
    -> [String]                             -- ^ Arguments
    -> IO (Handle, Handle, Handle, ProcessHandle)
    -- ^ (Input Handle, Output Handle, Error Handle, Process Handle)
openProcErr fpath args = do
    let procObj = (proc fpath args) {
            std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = CreatePipe,
            close_fds = True,
            use_process_jobs = True
        }

    (Just writeInpEnd, Just readOutEnd, Just readErrEnd, procHandle)
        <- createProcess procObj
    return (writeInpEnd, readOutEnd, readErrEnd, procHandle)

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
-- @since 0.1.0.0
withErrExe ::
    (IsStream t, MonadCatch m, MonadAsync m)
    => FilePath             -- ^ Path to Executable
    -> [String]             -- ^ Arguments
    -> Fold m Word8 b       -- ^ Fold to fold the error stream
    -> t m Word8            -- ^ Input stream to the process
    -> (Handle -> t m a)
    -- ^ Handle to read output of process and generate stream
    -> t m a                -- ^ Stream produced
withErrExe fpath args fld input genStrm = S.bracket pre post body

    where

    writeAction writeHdl =
        FH.fromBytes writeHdl (adapt input) >> liftIO (hClose writeHdl)

    foldErrAction errorHdl =
        S.fold fld (FH.toBytes errorHdl) >> liftIO (hClose errorHdl)

    runActions writeHdl errorHdl = parallel
        (S.nilM $ writeAction writeHdl)
        (S.nilM $ foldErrAction errorHdl)

    pre = liftIO $ openProcErr fpath args

    post (_, readHdl, _, procHandle) =
        liftIO $ hClose readHdl >> exceptOnError procHandle

    body (writeHdl, readHdl, errorHdl, _) =
        parallel (runActions writeHdl errorHdl) (genStrm readHdl)

-- |
-- Runs a process specified by the path to executable and arguments
-- that are to be passed and returns the output of the process
-- (standard output) in the form of a stream of Word8
--
-- Raises an exception 'ProcessFailure' if process failed due to some
-- reason
--
-- @since 0.1.0.0
{-# INLINE toBytes #-}
toBytes ::
    (IsStream t, MonadIO m, MonadCatch m)
    => FilePath     -- ^ Path to executable
    -> [String]     -- ^ Arguments to pass to executable
    -> t m Word8    -- ^ Output Stream
toBytes fpath args = AS.concat $ withExe fpath args FH.toChunks

-- |
-- Runs a process specified by the path to executable and arguments
-- that are to be passed and returns the output of the process
-- (standard output) in the form of a stream of array of Word8
--
-- Raises an exception 'ProcessFailure' if process failed due to some
-- reason
--
-- @since 0.1.0.0
toChunks ::
    (IsStream t, MonadIO m, MonadCatch m)
    => FilePath             -- ^ Path to executable
    -> [String]             -- ^ Arguments to pass to executable
    -> t m (Array Word8)    -- ^ Output Stream
toChunks fpath args = withExe fpath args FH.toChunks

-- |
-- Runs a process specified by the path to executable, arguments
-- that are to be passed and input to be provided to the process
-- (standard input) and returns the output of the process (standard output).
--
-- Raises an exception 'ProcessFailure' if process failed due to some
-- reason
--
-- @since 0.1.0.0
{-# INLINE processBytes_ #-}
processBytes_ ::
    (IsStream t, MonadCatch m, MonadAsync m)
    => FilePath     -- ^ Path to executable
    -> [String]     -- ^ Arguments to pass to executable
    -> t m Word8    -- ^ Input Stream
    -> t m Word8    -- ^ Output Stream
processBytes_ fpath args inStream =
    AS.concat $ withInpExe fpath args inStream FH.toChunks

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
-- @since 0.1.0.0
{-# INLINE processChunks_ #-}
processChunks_ ::
    (IsStream t, MonadCatch m, MonadAsync m)
    => FilePath             -- ^ Path to executable
    -> [String]             -- ^ Arguments to pass to executable
    -> t m (Array Word8)    -- ^ Input Stream
    -> t m (Array Word8)    -- ^ Output Stream
processChunks_ fpath args inStream =
    withInpExe fpath args (AS.concat inStream) FH.toChunks

-- |
-- Runs a process specified by the path to executable, arguments
-- that are to be passed and input to be provided to the process
-- (standard input) in the form of a stream of Word8 and folds
-- the error stream using the given Fold along with returning
-- the output of the process (standard output) in the form of a
-- stream of Word8
--
-- Raises an exception 'ProcessFailure' if process failed due to some
-- reason. The Fold would continue if you would catch the thrown exception.
--
-- @since 0.1.0.0
{-# INLINE processBytes #-}
processBytes ::
    (IsStream t, MonadCatch m, MonadAsync m)
    => FilePath         -- ^ Path to executable
    -> [String]         -- ^ Arguments to pass to executable
    -> Fold m Word8 b   -- ^ Fold to fold Error Stream
    -> t m Word8        -- ^ Input Stream
    -> t m Word8        -- ^ Output Stream
processBytes fpath args fld inStream =
    withErrExe fpath args fld inStream FH.toBytes

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
-- @since 0.1.0.0
{-# INLINE processChunks #-}
processChunks ::
    (IsStream t, MonadCatch m, MonadAsync m)
    => FilePath                 -- ^ Path to executable
    -> [String]                 -- ^ Arguments to pass to executable
    -> Fold m Word8 b           -- ^ Fold to fold Error Stream
    -> t m (Array Word8)        -- ^ Input Stream
    -> t m (Array Word8)        -- ^ Output Stream
processChunks fpath args fld inStream =
    withErrExe fpath args fld (AS.concat inStream) FH.toChunks
