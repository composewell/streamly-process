{-# LANGUAGE FlexibleContexts #-}

module Streamly.System.Process
    ( fromExe
    , fromExeChunks
    , thruExe_
    , thruExeChunks_
    )
where

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.Memory.ArrayStream as AS
import Streamly.Internal.Data.SVar (MonadAsync, fork)
import Streamly.Internal.Memory.Array.Types (Array)
import Streamly.Internal.Data.Stream.Serial (SerialT, serially)
import Streamly.Internal.Data.Stream.StreamK.Type 
    ( IsStream
    , fromStream
    , toStream
    , adapt
    )

import System.Exit (ExitCode(..))
import System.IO (hClose, Handle)
import System.Process
    ( ProcessHandle
    , CreateProcess(..)
    , StdStream (..)
    , createProcess
    , createPipe
    , proc
    , waitForProcess
    )

import Data.Word (Word8)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadCatch, throwM)
import Control.Exception (Exception, displayException)

-- ProcessFailed exception datatype
newtype ProcessFailed = ProcessFailed Int
    deriving Show

-- Exception instance of ProcessFailed
instance Exception ProcessFailed where

    displayException (ProcessFailed exitCodeInt) = 
        "Process Failed With Exit Code " ++ show exitCodeInt

exceptOnError :: (MonadIO m, MonadCatch m) => ProcessHandle -> m ()
exceptOnError procHandle = liftIO $ do
    exitCode <- waitForProcess procHandle
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure exitCodeInt -> throwM $ ProcessFailed exitCodeInt

{-# INLINE openProc #-}
openProc :: 
    FilePath                        -- ^ Path to Executable
    -> [String]                     -- ^ Arguments
    -> IO (Handle, ProcessHandle)   
    -- ^ Handle to read from output of process, process handle
openProc fpath args = do
    (readEnd, writeEnd) <- createPipe
    let 
        procObj = (proc fpath args) {
            std_out = UseHandle writeEnd,
            close_fds = True
        }

    (_, _, _, procHandle) <- createProcess procObj
    return (readEnd, procHandle)

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

    body (readHdl, procHandle) = genStrm readHdl

{-# INLINE openProcInp #-}
openProcInp ::  
    FilePath                                -- ^ Path to Executable
    -> [String]                             -- ^ Arguments
    -> IO (Handle, Handle, ProcessHandle)   
    -- ^ (Input Handle, Output Handle, Process Handle)
openProcInp fpath args = do
    (readInpEnd, writeInpEnd) <- createPipe
    (readOutEnd, writeOutEnd) <- createPipe
    let 
        procObj = (proc fpath args) {
            std_in = UseHandle readInpEnd, 
            std_out = UseHandle writeOutEnd,
            close_fds = True
        }

    (_, _, _, procHandle) <- createProcess procObj
    return (writeInpEnd, readOutEnd, procHandle)

{-# INLINE withInpExe #-}
withInpExe :: 
    (IsStream t, MonadCatch m, MonadIO m, MonadAsync m)
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

        post (writeHdl, readHdl, procHandle) = 
            liftIO $ hClose readHdl >> exceptOnError procHandle
        
        body (writeHdl, readHdl, procHandle) = 
            S.before (fork $ writeAction writeHdl) $ genStrm readHdl

{-# INLINE fromExe #-}
fromExe ::  
    (IsStream t, MonadIO m, MonadCatch m)
    => FilePath     -- ^ Path to executable
    -> [String]     -- ^ Arguments to pass to executable
    -> t m Word8    -- ^ Output Stream
fromExe fpath args = AS.concat $ withExe fpath args FH.toChunks

fromExeChunks ::    
    (IsStream t, MonadIO m, MonadCatch m)
    => FilePath             -- ^ Path to executable
    -> [String]             -- ^ Arguments to pass to executable
    -> t m (Array Word8)    -- ^ Output Stream
fromExeChunks fpath args = withExe fpath args FH.toChunks

{-# INLINE thruExe_ #-}
thruExe_ :: 
    (IsStream t, MonadIO m, MonadCatch m, MonadAsync m)
    => FilePath     -- ^ Path to executable
    -> [String]     -- ^ Arguments to pass to executable
    -> t m Word8    -- ^ Input Stream
    -> t m Word8    -- ^ Output Stream
thruExe_ fpath args inStream = 
    AS.concat $ withInpExe fpath args inStream FH.toChunks

{-# INLINE thruExeChunks_ #-}
thruExeChunks_ :: 
    (IsStream t, MonadIO m, MonadCatch m, MonadAsync m)
    => FilePath             -- ^ Path to executable
    -> [String]             -- ^ Arguments to pass to executable
    -> t m (Array Word8)    -- ^ Input Stream
    -> t m (Array Word8)    -- ^ Output Stream
thruExeChunks_ fpath args inStream = 
    withInpExe fpath args (AS.concat inStream) FH.toChunks
