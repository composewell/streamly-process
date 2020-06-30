module Main where

import qualified Streamly.Internal.Prelude as S
import qualified Streamly.System.Process as Proc
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.Memory.ArrayStream as AS

import System.IO (FilePath, Handle, IOMode(..), openFile, hClose)
import System.Process (proc, createProcess, waitForProcess)
import System.Directory (removeFile, findExecutable)

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

toUpper :: Word8 -> Word8
toUpper char =
    if(_a <= char && char <= _z)
    then (char - _a) + _A
    else char

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

charFile :: String
charFile = "./largeCharFile"

generateCharFile :: Int -> IO ()
generateCharFile numCharInCharFile = do
    handle <- openFile charFile WriteMode
    FH.fromBytes handle (S.replicate numCharInCharFile _a)
    hClose handle

toBytes :: Property
toBytes = 
    forAll (choose (minBlockCount, maxBlockCount)) $ \numBlock ->        
        monadicIO $ do
            catBinary <- run ioCatBinary
            let
                genStrm = Proc.toBytes catBinary [charFile]

                ioByteStrm = do
                    handle <- openFile charFile ReadMode
                    let strm = FH.toBytes handle
                    return $ S.finally (hClose handle) strm

            run $ generateCharFile numBlock
            byteStrm <- run ioByteStrm
            genList <- run $ S.toList genStrm
            byteList <- run $ S.toList byteStrm
            run $ removeFile charFile
            listEquals (==) genList byteList

toChunks :: Property
toChunks = 
    forAll (choose (minBlockCount, maxBlockCount)) $ \numBlock ->
        monadicIO $ do
            catBinary <- run ioCatBinary
            let
                genStrm = Proc.toChunks catBinary [charFile]
                
                ioByteStrm = do
                    handle <- openFile charFile ReadMode
                    let strm = FH.toBytes handle
                    return $ S.finally (hClose handle) strm

            run $ generateCharFile numBlock
            byteStrm <- run ioByteStrm
            genList <- run $ S.toList (AS.concat genStrm)
            byteList <- run $ S.toList byteStrm
            run $ removeFile charFile
            listEquals (==) genList byteList

transformBytes :: Property
transformBytes = 
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            trBinary <- run ioTrBinary
            let
                inputStream = S.fromList ls
                genStrm = Proc.transformBytes trBinary ["[a-z]", "[A-Z]"] inputStream
                charUpperStrm = S.map toUpper inputStream

            genList <- run $ S.toList genStrm
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

transformChunks :: Property
transformChunks = 
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            trBinary <- run ioTrBinary
            let
                inputStream = S.fromList ls
            
                genStrm = AS.concat $ 
                    Proc.transformChunks 
                    trBinary 
                    ["[a-z]", "[A-Z]"] 
                    (AS.arraysOf arrayChunkElem inputStream)
                
                charUpperStrm = S.map toUpper inputStream

            genList <- run $ S.toList genStrm
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

main :: IO ()
main = hspec $ do
    describe "test for process functions" $ do
        -- prop "Proc.toBytes cat = FH.toBytes" toBytes
        -- prop "Proc.toChunks cat = FH.toChunks" toChunks
        prop "transformBytes tr = map toUpper " transformBytes
        -- prop "AS.concat $ transformChunks tr = map toUpper " transformChunks
