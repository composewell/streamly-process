module Main where

import qualified Streamly.Internal.Prelude as S
import qualified Streamly.System.Process as Proc
import qualified Streamly.Internal.FileSystem.Handle as FH

import System.IO (FilePath, Handle, IOMode(..), openFile, hClose, stdout)
import System.Process (proc, createProcess, waitForProcess)
import System.Directory (removeFile, findExecutable)

import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.IORef (IORef (..), newIORef, readIORef, writeIORef)
import Data.Word (Word8)

import Gauge (Benchmarkable, defaultMain, bench, nfIO, perRunEnv, perRunEnvWithCleanup)

_a :: Word8
_a = 97

devRandom :: String
devRandom = "/dev/random"

devNull :: String
devNull = "/dev/null"

ioDdBinary :: IO FilePath
ioDdBinary = do
    maybeDdBinary <- findExecutable "dd"
    case maybeDdBinary of
        Just ddBinary -> return ddBinary
        _ -> error "dd Binary Not Found"

ddBlockSize :: Int
ddBlockSize = 1024

ddBlockCount :: Int
ddBlockCount = 100              -- ~100 KB

numCharInCharFile :: Int
numCharInCharFile = 100 * 1024  -- ~100 KB

ioCatBinary :: IO FilePath
ioCatBinary = do
    maybeDdBinary <- findExecutable "cat"
    case maybeDdBinary of
        Just ddBinary -> return ddBinary
        _ -> error "cat Binary Not Found"

ioTrBinary :: IO FilePath
ioTrBinary = do
    maybeDdBinary <- findExecutable "tr"
    case maybeDdBinary of
        Just ddBinary -> return ddBinary
        _ -> error "tr Binary Not Found"

largeByteFile :: String
largeByteFile = "./largeByteFile"

largeCharFile :: String
largeCharFile = "./largeCharFile"

generateByteFile :: IO ()
generateByteFile = 
    do
        ddBinary <- ioDdBinary
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

runCatCmd :: Handle -> IO ()
runCatCmd hdl = do
    catBinary <- ioCatBinary
    FH.fromBytes hdl $ Proc.toBytes catBinary [largeByteFile]

runCatCmdChunk :: Handle -> IO ()
runCatCmdChunk hdl = do
    catBinary <- ioCatBinary
    FH.fromChunks hdl $ 
        Proc.toChunks catBinary [largeByteFile]

runTrCmd :: (Handle, Handle) -> IO ()
runTrCmd (inputHdl, outputHdl) = do
    trBinary <- ioTrBinary
    FH.fromBytes outputHdl $ 
        Proc.transformBytes trBinary ["[a-z]", "[A-Z]"] (FH.toBytes inputHdl)

runTrCmdChunk :: (Handle, Handle) -> IO ()
runTrCmdChunk (inputHdl, outputHdl) = do
    trBinary <- ioTrBinary
    FH.fromChunks outputHdl $ 
        Proc.transformChunks trBinary ["[a-z]", "[A-Z]"] (FH.toChunks inputHdl)

benchWithOut :: IORef Handle -> (Handle -> IO ()) -> Benchmarkable
benchWithOut nullFileIoRef func = perRunEnv openNewHandle benchCode

    where
    
    openNewHandle = do
        oldHandle <- readIORef nullFileIoRef
        hClose oldHandle
        newHandle <- openFile devNull WriteMode
        writeIORef nullFileIoRef newHandle

    benchCode _ = do
        handle <- readIORef nullFileIoRef
        func handle

benchWithInpOut :: IORef (Handle, Handle) -> ((Handle, Handle) -> IO ()) -> Benchmarkable
benchWithInpOut inpOutIoRef func = perRunEnv openNewHandles benchCode

    where
    
    openNewHandles = do
        (oldInputHdl, oldOutputHdl) <- readIORef inpOutIoRef
        hClose oldInputHdl
        hClose oldOutputHdl
        newInputHdl <- openFile largeCharFile ReadMode
        newOutputHdl <- openFile devNull WriteMode
        writeIORef inpOutIoRef (newInputHdl, newOutputHdl)

    benchCode _ = do
        inpOutHdls <- readIORef inpOutIoRef
        func inpOutHdls

main :: IO ()
main = do
    generateFiles
    tempHandleWrite <- openFile devNull WriteMode
    tempHandleRead <- openFile devNull ReadMode
    ioRefOut <- newIORef tempHandleWrite
    ioRefInpOut <- newIORef (tempHandleRead, tempHandleWrite)
    defaultMain [
            bench "exe - word8" $ 
                benchWithOut ioRefOut runCatCmd,
            bench "exe - array of word8" $
                benchWithOut ioRefOut runCatCmdChunk,
            bench "exe - word8 to word8" $ 
                benchWithInpOut ioRefInpOut runTrCmd,
            bench "exe - array of word8 to array of word8" $
                benchWithInpOut ioRefInpOut runTrCmdChunk
        ]
    handleOut1 <- readIORef ioRefOut
    hClose handleOut1
    (handleIn2, handleOut2) <- readIORef ioRefInpOut
    hClose handleIn2
    hClose handleOut2
    deleteFiles
