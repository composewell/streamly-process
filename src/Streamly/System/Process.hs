module Streamly.System.Process (
    fromExe,
    fromExeChunks,
    thruExe_,
    thruExeChunks_
) where

import Streamly.Prelude ((.:))
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.FileSystem.Handle as FH
import Streamly.Internal.Memory.Array.Types (Array)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream, fromStream, toStream, adapt)
import Streamly.Internal.Data.Stream.Serial (SerialT, serially)

import System.Process
import System.Exit (ExitCode(..))
import System.IO (hClose)

import Data.Word (Word8)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadCatch, throwM)
import Control.Exception (Exception, displayException)

-- ProcessFailed exception datatype
newtype ProcessFailed = ProcessFailed Int
    deriving Show

-- Exception instance of ProcessFailed
instance Exception ProcessFailed where

    displayException (ProcessFailed exitCodeInt) = "Process Failed With Exit Code " ++ show exitCodeInt

exceptOnError :: (MonadIO m, MonadCatch m) => ProcessHandle -> m ()
exceptOnError procHandle = liftIO $ do
    exitCode <- waitForProcess procHandle
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure exitCodeInt -> throwM $ ProcessFailed exitCodeInt

fromExe ::  (IsStream t, MonadIO m, MonadCatch m) =>
            FilePath ->         -- Path to executable
            [String] ->         -- Arguments to pass to executable
            t m Word8           -- Output Stream

fromExe fpath argList = 
    let
        ioOutStream = do
            (stdOutReadEnd, stdOutWriteEnd) <- createPipe
            
            let 
                procObj = (proc fpath argList) {std_out = UseHandle stdOutWriteEnd}
                closeHdl = liftIO $ (hClose stdOutReadEnd >> hClose stdOutWriteEnd)
                onCompletionAction procHandle = closeHdl >> (exceptOnError procHandle)

            (_, _, _, procHandle) <- createProcess procObj

            return $ S.finally (onCompletionAction procHandle) (FH.toBytes stdOutReadEnd)
    in
        S.concatM (liftIO ioOutStream)

fromExeChunks ::    (IsStream t, MonadIO m, MonadCatch m) =>
                    FilePath ->         -- Path to executable
                    [String] ->         -- Arguments to pass to executable
                    t m (Array Word8)   -- Output Stream

fromExeChunks fpath argList = 
    let
        ioOutStream = do
            (stdOutReadEnd, stdOutWriteEnd) <- createPipe
            
            let 
                procObj = (proc fpath argList) {std_out = UseHandle stdOutWriteEnd}
                closeHdl = liftIO $ (hClose stdOutReadEnd >> hClose stdOutWriteEnd)
                onCompletionAction procHandle = closeHdl >> (exceptOnError procHandle)

            (_, _, _, procHandle) <- createProcess procObj

            return $ S.finally (onCompletionAction procHandle) (FH.toChunks stdOutReadEnd)
    in
        S.concatM (liftIO ioOutStream)

thruExe_ :: (IsStream t, MonadIO m, MonadCatch m) =>
            FilePath -> 
            [String] -> 
            t m Word8 ->    -- Input Stream
            t m Word8       -- Output Stream

thruExe_ fpath argList inStream = 
    let
        ioOutStream = do
            (stdInReadEnd, stdInWriteEnd) <- liftIO createPipe
            (stdOutReadEnd, stdOutWriteEnd) <- liftIO createPipe
            
            let 
                procObj = (proc fpath argList) {std_in = UseHandle stdInReadEnd, std_out = UseHandle stdOutWriteEnd}
                closeHdl = liftIO $ (hClose stdOutReadEnd >> hClose stdOutWriteEnd >> hClose stdOutReadEnd >> hClose stdOutWriteEnd)
                onCompletionAction procHandle = closeHdl >> (exceptOnError procHandle)

            FH.fromBytes stdInWriteEnd $ adapt inStream                     -- Write input stream to read end of std-in pipe
            (_, _, _, procHandle) <- liftIO $ createProcess procObj         -- Create the Process
            
            return $ S.finally (onCompletionAction procHandle) (FH.toBytes stdOutReadEnd)
    in
        S.concatM ioOutStream

thruExeChunks_ :: (IsStream t, MonadIO m, MonadCatch m) =>
            FilePath -> 
            [String] -> 
            t m (Array Word8) ->    -- Input Stream
            t m (Array Word8)       -- Output Stream

thruExeChunks_ fpath argList inStream = 
    let
        ioOutStream = do
            (stdInReadEnd, stdInWriteEnd) <- liftIO createPipe
            (stdOutReadEnd, stdOutWriteEnd) <- liftIO createPipe
            
            let 
                procObj = (proc fpath argList) {std_in = UseHandle stdInReadEnd, std_out = UseHandle stdOutWriteEnd}
                closeHdl = liftIO $ (hClose stdOutReadEnd >> hClose stdOutWriteEnd >> hClose stdOutReadEnd >> hClose stdOutWriteEnd)
                onCompletionAction procHandle = closeHdl >> (exceptOnError procHandle)

            FH.fromChunks stdInWriteEnd $ adapt inStream                        -- Write input stream to read end of std-in pipe
            (_, _, _, procHandle) <- liftIO $ createProcess procObj             -- Create the Process
            
            return $ S.finally (onCompletionAction procHandle) (FH.toChunks stdOutReadEnd)
    in
        S.concatM ioOutStream