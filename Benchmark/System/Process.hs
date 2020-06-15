module Main where

import Streamly.System.Process as Proc
import Streamly.Internal.FileSystem.Handle as FH

import System.IO (openFile, hClose, IOMode(..))

import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Gauge

devNull :: String
devNull = "/dev/null"

catBinary :: String
catBinary = "/bin/cat"

trBinary :: String
trBinary = "/usr/bin/tr"

largeFile :: String
largeFile = "/Users/ahmed/Downloads/largeFile"

smallFile :: String
smallFile = "/Users/ahmed/Downloads/smallFile"

runCatCmd :: IO ()
runCatCmd = do
    hdl <- openFile devNull WriteMode
    FH.fromBytes hdl $ Proc.fromExe catBinary [largeFile]
    hClose hdl

runCatCmdChunk :: IO ()
runCatCmdChunk = do
    hdl <- openFile devNull WriteMode
    FH.fromChunks hdl $ Proc.fromExeChunks catBinary [largeFile]
    hClose hdl

runTrCmd :: IO ()
runTrCmd = do
    inputHdl <- openFile largeFile ReadMode
    outputHdl <- openFile devNull WriteMode
    FH.fromBytes outputHdl $ Proc.thruExe_ trBinary ["[a-z]", "[A-Z]"] (FH.toBytes inputHdl)
    hClose inputHdl
    hClose outputHdl

runTrCmdChunk :: IO ()
runTrCmdChunk = do
    inputHdl <- openFile largeFile ReadMode
    outputHdl <- openFile devNull WriteMode
    FH.fromChunks outputHdl $ Proc.thruExeChunks_ trBinary ["[a-z]", "[A-Z]"] (FH.toChunks inputHdl)
    hClose inputHdl
    hClose outputHdl

main :: IO ()
main = defaultMain [
        bench "exe - word8" $ nfIO runCatCmd,
        bench "exe - array of word8" $ nfIO runCatCmdChunk,
        bench "exe - word8 to word8" $ nfIO runTrCmd,
        bench "exe - array of word8 to array of word8" $ nfIO runTrCmdChunk
    ]