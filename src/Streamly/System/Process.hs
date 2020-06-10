module Streamly.System.Process (
    fromExe
) where

import Streamly.Prelude ((.:))
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.FileSystem.Handle as FH
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream, fromStream, toStream)

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

fromExe ::  (IsStream t, MonadIO m, MonadCatch m) =>
            FilePath ->         -- Path to executable
            [String] ->         -- Arguments to pass to executable
            t m Word8           -- Output Stream

fromExe fpath argList = 
    let
        ioOutStream = do
            (stdOutReadEnd, stdOutWriteEnd) <- createPipe
            
            let procObj = (proc fpath argList) {std_out = UseHandle stdOutWriteEnd}
            (_, _, _, procHandle) <- createProcess procObj
            
            let
                onCompletionAction = closeFile >> exceptOnError
                    where
                        closeFile = liftIO $ (hClose stdOutReadEnd >> hClose stdOutWriteEnd)
                        exceptOnError = liftIO $ do
                            exitCode <- waitForProcess procHandle
                            case exitCode of
                                ExitSuccess -> return ()
                                ExitFailure exitCodeInt -> throwM $ ProcessFailed exitCodeInt

            return $ S.finally onCompletionAction (FH.toBytes stdOutReadEnd)
    in
        S.concatM (liftIO ioOutStream)