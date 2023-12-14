{-# LANGUAGE  ScopedTypeVariables #-}
{-# OPTIONS_GHC -fspec-constr-recursive=4 #-}

module Main (main) where

import Control.Exception (finally)
import Data.Either (isRight, fromRight, isLeft, fromLeft)
import Data.Word (Word8)
import Test.Tasty.Bench (defaultMain, bench, nfIO)
import System.Directory (removeFile, findExecutable)
import System.IO
    ( Handle
    , IOMode(..)
    , openFile
    , hClose
    )
import Streamly.Data.Stream (Stream)

import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Stream as S
import qualified Streamly.System.Process as Proc
import qualified Streamly.Internal.System.Command as Cmd

-- Internal imports
import qualified Streamly.Internal.FileSystem.Handle as FH

-- XXX replace with streamly versions once they are fixed
{-# INLINE rights #-}
rights :: Monad m => Stream m (Either a b) -> Stream m b
rights = fmap (fromRight undefined) . S.filter isRight

{-# INLINE lefts #-}
lefts :: Monad m => Stream m (Either a b) -> Stream m a
lefts = fmap (fromLeft undefined) . S.filter isLeft

-------------------------------------------------------------------------------
-- Constants and utils
-------------------------------------------------------------------------------

_a :: Word8
_a = 97

-- XXX portability on macOS
devRandom :: String
devRandom = "/dev/urandom"

devNull :: String
devNull = "/dev/null"

which :: String -> IO FilePath
which cmd = do
    r <- findExecutable cmd
    case r of
        Just path -> return path
        _ -> error $ "Required command " ++ cmd ++ " not found"

-------------------------------------------------------------------------------
-- Create a data file filled with random data
-------------------------------------------------------------------------------

ddBlockSize :: Int
ddBlockSize = 1024 * 1024

ddBlockCount :: Int
ddBlockCount = 10

largeByteFile :: String
largeByteFile = "./largeByteFile"

generateByteFile :: IO ()
generateByteFile = do
    ddPath <- which "dd"
    Cmd.toStdout
        $ ddPath
            ++ " if=" ++ devRandom
            ++ " of=" ++ largeByteFile
            ++ " count=" ++ show ddBlockCount
            ++ " bs=" ++ show ddBlockSize

-------------------------------------------------------------------------------
-- Create a file filled with ascii chars
-------------------------------------------------------------------------------

largeCharFile :: String
largeCharFile = "./largeCharFile"

numCharInCharFile :: Int
numCharInCharFile = 10 * 1024 * 1024

generateCharFile :: IO ()
generateCharFile = do
    handle <- openFile largeCharFile WriteMode
    FH.putBytes handle (S.replicate numCharInCharFile _a)
    hClose handle

-------------------------------------------------------------------------------
-- Create a utility that writes to stderr
-------------------------------------------------------------------------------

trToStderr :: String
trToStderr = "./writeTrToError.sh"

trToStderrContent :: String
trToStderrContent =
    "#!/bin/sh\ntr [a-z] [A-Z] <&0 >&2"

createExecutable :: IO ()
createExecutable = do
    writeFile trToStderr trToStderrContent
    Cmd.toStdout ("chmod +x " ++ trToStderr)

-------------------------------------------------------------------------------
-- Create and delete the temp data/exec files
-------------------------------------------------------------------------------

generateFiles :: IO ()
generateFiles = do
    createExecutable
    generateByteFile
    generateCharFile

deleteFiles :: IO ()
deleteFiles = do
    removeFile trToStderr
    removeFile largeByteFile
    removeFile largeCharFile

-------------------------------------------------------------------------------
-- Benchmark functions
-------------------------------------------------------------------------------

toBytesEither :: String-> Handle -> IO ()
toBytesEither catPath outH =
    FH.putBytes outH
        $ rights
        $ Proc.toBytesEither catPath [largeByteFile]

toChunksEither :: String -> Handle -> IO ()
toChunksEither catPath hdl =
    FH.putChunks hdl
        $ rights
        $ Proc.toChunksEither catPath [largeByteFile]

pipeBytesEither :: String-> Handle -> IO ()
pipeBytesEither trPath outputHdl = do
    inputHdl <- openFile largeCharFile ReadMode
    _ <- S.fold (FL.partition (FH.write outputHdl) (FH.write outputHdl))
        $ Proc.pipeBytesEither
            trPath
            ["[a-z]", "[A-Z]"]
        $ FH.read inputHdl
    hClose inputHdl

pipeBytes :: String-> Handle -> IO ()
pipeBytes trPath outputHdl = do
    inputHdl <- openFile largeCharFile ReadMode
    FH.putBytes outputHdl
        $ Proc.pipeBytes
            trPath
            ["[a-z]", "[A-Z]"]
        $ FH.read inputHdl
    hClose inputHdl

processBytesToStderr :: Handle -> IO ()
processBytesToStderr outputHdl = do
    inputHdl <- openFile largeCharFile ReadMode
    FH.putBytes outputHdl
        $ lefts
        $ Proc.pipeBytesEither
            trToStderr
            ["[a-z]", "[A-Z]"]
        $ FH.read inputHdl
    hClose inputHdl

pipeChunks :: String -> Handle -> IO ()
pipeChunks trPath outputHdl = do
    inputHdl <- openFile largeCharFile ReadMode
    FH.putChunks outputHdl $
        Proc.pipeChunks
            trPath
            ["[a-z]", "[A-Z]"]
        $ FH.readChunks inputHdl
    hClose inputHdl

pipeChunksEither :: String -> Handle -> IO ()
pipeChunksEither trPath outputHdl = do
    inputHdl <- openFile largeCharFile ReadMode
    _ <- S.fold
            (FL.partition
                (FH.writeChunks outputHdl) (FH.writeChunks outputHdl)
            )
        $ Proc.pipeChunksEither
            trPath
            ["[a-z]", "[A-Z]"]
            (FH.readChunks inputHdl)
    hClose inputHdl

processChunksToStderr :: Handle -> IO ()
processChunksToStderr outputHdl = do
    inputHdl <- openFile largeCharFile ReadMode
    FH.putChunks outputHdl
        $ lefts
        $ Proc.pipeChunksEither
            trToStderr
            ["[a-z]", "[A-Z]"]
            (FH.readChunks inputHdl)
    hClose inputHdl

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Generating files..."
    generateFiles
    trPath <- which "tr"
    catPath <- which "cat"
    nullH <- openFile devNull WriteMode
    putStrLn "Running benchmarks..."

    defaultMain
        [ bench "toBytesEither" $ nfIO $ toBytesEither catPath nullH
        , bench "toChunksEither" $ nfIO $ toChunksEither catPath nullH
        , bench "pipeBytes tr" $ nfIO $ pipeBytes trPath nullH
        , bench "pipeBytesEither tr" $ nfIO $ pipeBytesEither trPath nullH
        , bench "processBytesToStderr tr" $ nfIO $ processBytesToStderr nullH
        , bench "pipeChunks tr" $ nfIO (pipeChunks trPath nullH)
        , bench "pipeChunksEither tr" $ nfIO (pipeChunksEither trPath nullH)
        , bench "processChunksToStderr" $ nfIO $ processChunksToStderr nullH
        ] `finally` (do
            putStrLn "cleanup ..."
            hClose nullH
            deleteFiles
           )
