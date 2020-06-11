module Main where

import Streamly.System.Process as Proc
import Streamly.Internal.FileSystem.Handle as FH

import System.IO (openFile, hClose, IOMode(..))

import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Gauge

runCatCmd :: IO ()
runCatCmd = do
    hdl <- openFile "/dev/null" WriteMode
    FH.fromBytes hdl $ Proc.fromExe "/bin/cat" ["/Users/ahmed/Downloads/largeFile"]
    hClose hdl

runCatCmdChunk :: IO ()
runCatCmdChunk = do
    hdl <- openFile "/dev/null" WriteMode
    FH.fromChunks hdl $ Proc.fromExeChunks "/bin/cat" ["/Users/ahmed/Downloads/largeFile"]
    hClose hdl

main :: IO ()
main = defaultMain [
        bench "exe - word8" $ nfIO runCatCmd,
        bench "exe - array of word8" $ nfIO runCatCmdChunk
    ]