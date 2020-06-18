module Main where

import qualified Streamly.Internal.Prelude as S
import qualified Streamly.System.Process as Proc
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.Memory.ArrayStream as AS

import System.IO (Handle, IOMode(..), openFile, hClose)
import System.Process (proc, createProcess, waitForProcess)
import System.Directory (removeFile)

import Test.Hspec(hspec, describe)
import Test.Hspec.QuickCheck
import Test.QuickCheck 
    ( forAll
    , choose
    , Property
    , property
    , listOf
    , vectorOf
    , counterexample
    , Gen
    )

import Test.QuickCheck.Monadic (monadicIO, PropertyM, assert, monitor, run)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)

import Data.List ((\\))
import Data.Word (Word8)

_a :: Word8
_a = 97

_z :: Word8
_z = 122

_A :: Word8
_A = 65

_Z :: Word8
_Z = 90

toUpper :: Word8 -> Word8
toUpper char =
    if(_a <= char && char <= _z)
    then (char - _a) + _A
    else char

devRandom :: String
devRandom = "/dev/random"

devNull :: String
devNull = "/dev/null"

ddBinary :: String
ddBinary = "/bin/dd"

ddBlockSize :: Int
ddBlockSize = 1024

catBinary :: String
catBinary = "/bin/cat"

trBinary :: String
trBinary = "/usr/bin/tr"

minBlockCount :: Int
minBlockCount = 1

maxBlockCount :: Int
maxBlockCount = 100

minNumChar :: Int
minNumChar = 1

maxNumChar :: Int
maxNumChar = 100 * 1024

arrayChunkElem :: Int
arrayChunkElem = 100

listEquals :: (Show a, Eq a, MonadIO m)
    => ([a] -> [a] -> Bool) -> [a] -> [a] -> PropertyM m ()
listEquals eq process_output list = do
    when (not $ process_output `eq` list) $ liftIO $ putStrLn $
                  "process output " ++ show process_output
             ++ "\nlist   " ++ show list
             ++ "\nprocess output \\\\ list " ++ show (process_output \\ list)
             ++ "\nlist \\\\ process output " ++ show (list \\ process_output)
    when (not $ process_output `eq` list) $
        monitor
            (counterexample $
                  "process output " ++ show process_output
             ++ "\nlist   " ++ show list
             ++ "\nprocess output \\\\ list " ++ show (process_output \\ list)
             ++ "\nlist \\\\ process output " ++ show (list \\ process_output)
             )
    assert (process_output `eq` list)

byteFile :: String
byteFile = "./byteFile"

charFile :: String
charFile = "./largeCharFile"

generateByteFile :: Int -> IO ()
generateByteFile blockCount = 
    let
        procObj = proc ddBinary [
                "if=" ++ devRandom,
                "of=" ++ byteFile,
                "count=" ++ show blockCount,
                "bs=" ++ show ddBlockSize
            ]

    in
        do
            (_, _, _, procHandle) <- createProcess procObj
            waitForProcess procHandle
            return ()

generateCharFile :: Int -> IO ()
generateCharFile numCharInCharFile = do
    handle <- openFile charFile WriteMode
    FH.fromBytes handle (S.replicate numCharInCharFile _a)
    hClose handle

fromExe :: Property
fromExe = 
    forAll (choose (minBlockCount, maxBlockCount)) $ \numBlock ->
        let
            genStrm = Proc.fromExe catBinary [byteFile]
                
            ioByteStrm = do
                handle <- openFile byteFile ReadMode
                let strm = FH.toBytes handle
                return $ S.finally (hClose handle) strm
        in
            monadicIO $ do
                run $ generateByteFile numBlock
                byteStrm <- run ioByteStrm
                genList <- run $ S.toList genStrm
                byteList <- run $ S.toList byteStrm
                run $ removeFile byteFile
                listEquals (==) genList byteList

fromExeChunks :: Property
fromExeChunks = 
    forAll (choose (minBlockCount, maxBlockCount)) $ \numBlock ->
        let
            genStrm = Proc.fromExeChunks catBinary [byteFile]
                
            ioByteStrm = do
                handle <- openFile byteFile ReadMode
                let strm = FH.toChunks handle
                return $ S.finally (hClose handle) strm
        in
            monadicIO $ do
                run $ generateByteFile numBlock
                byteStrm <- run ioByteStrm
                genList <- run $ S.toList genStrm
                byteList <- run $ S.toList byteStrm
                run $ removeFile byteFile
                listEquals (==) genList byteList

thruExe_ :: Property
thruExe_ = 
    forAll (listOf (choose(_a, _z))) $ \ls ->
        let
            inputStream = S.fromList ls
            genStrm = Proc.thruExe_ trBinary ["[a-z]", "[A-Z]"] inputStream
            charUpperStrm = S.map toUpper inputStream
        in
            monadicIO $ do
                genList <- run $ S.toList genStrm
                charList <- run $ S.toList charUpperStrm
                listEquals (==) genList charList

thruExeChunks_ :: Property
thruExeChunks_ = 
    forAll (listOf (choose(_a, _z))) $ \ls ->
        let
            inputStream = S.fromList ls
            
            genStrm = AS.concat $ 
                Proc.thruExeChunks_ 
                trBinary 
                ["[a-z]", "[A-Z]"] 
                (AS.arraysOf arrayChunkElem inputStream)
            
            charUpperStrm = S.map toUpper inputStream
        in
            monadicIO $ do
                genList <- run $ S.toList genStrm
                charList <- run $ S.toList charUpperStrm
                listEquals (==) genList charList

main :: IO ()
main = hspec $ do
    describe "test for process functions" $ do
        prop "fromExe cat = toBytes" fromExe
        -- prop "fromExeChunks cat = toChunks" fromExeChunks
        prop "thruExe_ tr = map toUpper " thruExe_
        prop "AS.concat $ thruExeChunks_ tr = map toUpper " thruExeChunks_
