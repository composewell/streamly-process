module Main where

import qualified Streamly.Internal.Prelude as S
import qualified Streamly.System.Process as Proc
import qualified Streamly.Internal.FileSystem.Handle as FH

import System.IO (Handle, IOMode(..), openFile, hClose)
import System.Process (proc, createProcess, waitForProcess)
import System.Directory (removeFile)

import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.IORef (IORef (..), newIORef, readIORef, writeIORef)
import Data.Word (Word8)

import Gauge (Benchmarkable, defaultMain, bench, nfIO, perRunEnvWithCleanup)

_a :: Word8
_a = 97

devRandom :: String
devRandom = "/dev/random"

devNull :: String
devNull = "/dev/null"

ddBinary :: String
ddBinary = "/bin/dd"

ddBlockSize :: Int
ddBlockSize = 1024

ddBlockCount :: Int
ddBlockCount = 100              -- ~100 KB

numCharInCharFile :: Int
numCharInCharFile = 100 * 1024  -- ~100 KB

catBinary :: String
catBinary = "/bin/cat"

trBinary :: String
trBinary = "/usr/bin/tr"

largeByteFile :: String
largeByteFile = "./largeByteFile"

largeCharFile :: String
largeCharFile = "./largeCharFile"

generateByteFile :: IO ()
generateByteFile = 
    do
        let procObj = proc ddBinary [
                    "if=" ++ devRandom,
                    "of=" ++ largeByteFile,
                    "count=" ++ show ddBlockCount,
                    "bs=" ++ show ddBlockSize
                ]

        (_, _, _, procHandle) <- createProcess procObj
        waitForProcess procHandle
        return ()

generateCharFile :: IO ()
generateCharFile = do
    handle <- openFile largeCharFile WriteMode
    FH.fromBytes handle (S.replicate numCharInCharFile _a)
    hClose handle

generateFiles :: IO ()
generateFiles = generateByteFile >> generateCharFile

deleteFiles :: IO ()
deleteFiles = 
    removeFile largeByteFile >> removeFile largeCharFile

runCatCmd :: IORef Handle -> IO ()
runCatCmd ioRefHandle = do
    hdl <- readIORef ioRefHandle
    FH.fromBytes hdl $ Proc.fromExe catBinary [largeByteFile]

runCatCmdChunk :: IORef Handle -> IO ()
runCatCmdChunk ioRefHandle = do
    hdl <- readIORef ioRefHandle
    FH.fromChunks hdl $ 
        Proc.fromExeChunks catBinary [largeByteFile]

runTrCmd :: IORef (Handle, Handle) -> IO ()
runTrCmd ioRefHandles = do
    (inputHdl, outputHdl) <- readIORef ioRefHandles
    FH.fromBytes outputHdl $ 
        Proc.thruExe_ trBinary ["[a-z]", "[A-Z]"] (FH.toBytes inputHdl)

runTrCmdChunk :: IORef (Handle, Handle) -> IO ()
runTrCmdChunk ioRefHandles = do
    (inputHdl, outputHdl) <- readIORef ioRefHandles
    FH.fromChunks outputHdl $ 
        Proc.thruExeChunks_ trBinary ["[a-z]", "[A-Z]"] (FH.toChunks inputHdl)

benchWithOut :: (IORef Handle -> IO ()) -> Benchmarkable
benchWithOut func = perRunEnvWithCleanup envIO closeHandle func

    where 
    
    envIO = do
        outputHdl <- openFile devNull WriteMode
        newIORef outputHdl

    closeHandle ioRefOutputHdl = do
        outputHdl <- readIORef ioRefOutputHdl
        hClose outputHdl

benchWithInpOut :: (IORef (Handle, Handle) -> IO ()) -> Benchmarkable
benchWithInpOut func = perRunEnvWithCleanup envIO closeHandles func

    where 
    
    envIO = do 
        inputHdl <- openFile largeCharFile ReadMode
        outputHdl <- openFile devNull WriteMode
        newIORef (inputHdl, outputHdl)

    closeHandles ioRefHandles = do
        (inputHdl, outputHdl) <- readIORef ioRefHandles
        hClose inputHdl
        hClose outputHdl

main :: IO ()
main = do
    generateFiles
    defaultMain [
            bench "exe - word8" $ benchWithOut runCatCmd,
            bench "exe - array of word8" $ benchWithOut runCatCmdChunk,
            bench "exe - word8 to word8" $ benchWithInpOut runTrCmd,
            bench "exe - array of word8 to array of word8" $
                benchWithInpOut runTrCmdChunk
        ]
    deleteFiles
