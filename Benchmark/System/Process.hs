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

largeFile :: String
largeFile = "/Users/ahmed/Downloads/largeFile"

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

main :: IO ()
main = defaultMain [
        bench "exe - word8" $ nfIO runCatCmd,
        bench "exe - array of word8" $ nfIO runCatCmdChunk
    ]