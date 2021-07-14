{-# LANGUAGE CPP #-}

module Main where

import Data.List ((\\))
import Data.Word (Word8)
import Control.Exception (Exception)
import Control.Monad (unless)
import Control.Monad.Catch (throwM, catch)
import Control.Monad.IO.Class (MonadIO(..))
import Streamly.System.Process (ProcessFailure (..))
import System.Directory (removeFile, findExecutable)
import System.IO (IOMode(..), openFile, hClose)
import System.Process (callCommand)
import Test.Hspec (hspec, describe)
import Test.Hspec.QuickCheck
import Test.QuickCheck
    ( forAll
    , choose
    , Property
    , listOf
    , counterexample
    )
import Test.QuickCheck.Monadic (monadicIO, PropertyM, assert, monitor, run)

import qualified Streamly.Prelude as S
import qualified Streamly.System.Process as Proc

-- Internal imports
import qualified Streamly.Internal.Data.Array.Stream.Foreign
    as AS (arraysOf, concat)
import qualified Streamly.Internal.Data.Stream.IsStream as S (nilM, lefts, rights)
import qualified Streamly.Internal.FileSystem.Handle as FH (putBytes, toBytes)

newtype SimpleError = SimpleError String
    deriving Show

instance Exception SimpleError

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
    maybeCatBinary <- findExecutable "cat"
    case maybeCatBinary of
        Just catBinary -> return catBinary
        _ -> error "cat Binary Not Found"

ioTrBinary :: IO FilePath
ioTrBinary = do
    maybeTrBinary <- findExecutable "tr"
    case maybeTrBinary of
        Just trBinary -> return trBinary
        _ -> error "tr Binary Not Found"

failErrorMessage :: String
failErrorMessage = "fail"

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

executableFile :: FilePath
#if mingw32_HOST_OS == 1
executableFile = "./writeTrToError.bat"
#else
executableFile = "./writeTrToError.sh"
#endif

executableFileContent :: String
#if mingw32_HOST_OS == 1
executableFileContent = "@echo off\r\ntr [a-z] [A-Z] >&2\r\n"
#else
executableFileContent =
    "tr [a-z] [A-Z] >&2"
#endif

executableFileFail :: FilePath
#if mingw32_HOST_OS == 1
executableFileFail = "./failExec.bat"
#else
executableFileFail = "./failExec.sh"
#endif

executableFileFailContent :: String
executableFileFailContent =
    "exit 1"

executableFilePass :: FilePath
#if mingw32_HOST_OS == 1
executableFilePass = "./passExec.bat"
#else
executableFilePass = "./passExec.sh"
#endif

executableFilePassContent :: String
executableFilePassContent =
    "exit 0"

createExecutable :: FilePath -> String -> IO ()
createExecutable file content = do
    writeFile file content
    callCommand ("chmod +x " ++ file)

createExecutables :: IO ()
createExecutables = do
    createExecutable executableFile executableFileContent
    createExecutable executableFileFail executableFileFailContent
    createExecutable executableFilePass executableFilePassContent

removeExecutables :: IO ()
removeExecutables = do
    removeFile executableFile
    removeFile executableFileFail
    removeFile executableFilePass

toUpper :: Word8 -> Word8
toUpper char =
    if _a <= char && char <= _z
    then (char - _a) + _A
    else char

listEquals :: (Show a, Eq a, MonadIO m)
    => ([a] -> [a] -> Bool) -> [a] -> [a] -> PropertyM m ()
listEquals eq process_output list = do
    unless (process_output `eq` list) $ liftIO $ putStrLn $
                  "process output " ++ show process_output
             ++ "\nlist   " ++ show list
             ++ "\nprocess output \\\\ list " ++ show (process_output \\ list)
             ++ "\nlist \\\\ process output " ++ show (list \\ process_output)
    unless (process_output `eq` list) $
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
    FH.putBytes handle (S.replicate numCharInCharFile _a)
    hClose handle

toBytes1 :: Property
toBytes1 =
    forAll (choose (minBlockCount, maxBlockCount)) $ \numBlock ->
        monadicIO $ do
            catBinary <- run ioCatBinary
            let
                genStrm = S.rights $ Proc.toBytes' catBinary [charFile]

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

toBytes2 :: Property
toBytes2 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $ Proc.toBytes' executableFileFail []
        return False

    failAction (ProcessFailure exitCode) =
        return (exitCode == 1)

    checkFailAction = catch action failAction

toChunks1 :: Property
toChunks1 =
    forAll (choose (minBlockCount, maxBlockCount)) $ \numBlock ->
        monadicIO $ do
            catBinary <- run ioCatBinary
            let
                genStrm = S.rights $ Proc.toChunks' catBinary [charFile]

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

toChunks2 :: Property
toChunks2 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $ Proc.toChunks' executableFileFail []
        return False

    failAction (ProcessFailure exitCode) =
        return (exitCode == 1)

    checkFailAction = catch action failAction

processBytes1 :: Property
processBytes1 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            trBinary <- run ioTrBinary
            let
                inputStream = S.fromList ls
                genStrm = Proc.processBytes
                            trBinary
                            ["[a-z]", "[A-Z]"]
                            inputStream
                charUpperStrm = S.map toUpper inputStream

            genList <- run $ S.toList genStrm
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

processBytes2 :: Property
processBytes2 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $ Proc.processBytes executableFileFail [] S.nil
        return False

    failAction (ProcessFailure exitCode) =
        return (exitCode == 1)

    checkFailAction = catch action failAction

processBytes3 :: Property
processBytes3 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $
            Proc.processBytes
            executableFilePass
            []
            (S.nilM $ throwM (SimpleError failErrorMessage))
        return False

    failAction (SimpleError err) =
        return (err == failErrorMessage)

    checkFailAction = catch action failAction

processChunks1 :: Property
processChunks1 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            trBinary <- run ioTrBinary
            let
                inputStream = S.fromList ls

                genStrm = AS.concat $
                    Proc.processChunks
                    trBinary
                    ["[a-z]", "[A-Z]"]
                    (AS.arraysOf arrayChunkElem inputStream)

                charUpperStrm = S.map toUpper inputStream

            genList <- run $ S.toList genStrm
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

processChunks2 :: Property
processChunks2 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $ Proc.processChunks executableFileFail [] S.nil
        return False

    failAction (ProcessFailure exitCode) =
        return (exitCode == 1)

    checkFailAction = catch action failAction

processChunks3 :: Property
processChunks3 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $
            Proc.processChunks
            executableFilePass
            []
            (S.nilM $ throwM (SimpleError failErrorMessage))
        return False

    failAction (SimpleError err) =
        return (err == failErrorMessage)

    checkFailAction = catch action failAction

processBytes'1 :: Property
processBytes'1 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            trBinary <- run ioTrBinary
            let
                inputStream = S.fromList ls
                genStrm = S.rights $ Proc.processBytes'
                            trBinary
                            ["[a-z]", "[A-Z]"]
                            inputStream
                charUpperStrm = S.map toUpper inputStream

            genList <- run $ S.toList genStrm
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

processBytes'2 :: Property
processBytes'2 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            let
                inputStream = S.fromList ls
                outStream = S.lefts $
                    Proc.processBytes'
                    executableFile
                    ["[a-z]", "[A-Z]"]
                    inputStream

                charUpperStrm = S.map toUpper inputStream

            genList <- run $ S.toList outStream
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

processBytes'3 :: Property
processBytes'3 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $ Proc.processBytes' executableFileFail [] S.nil
        return False

    failAction (ProcessFailure exitCode) =
        return (exitCode == 1)

    checkFailAction = catch action failAction

processBytes'4 :: Property
processBytes'4 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $
            Proc.processBytes'
            executableFilePass
            []
            (S.nilM $ throwM (SimpleError failErrorMessage))
        return False

    failAction (SimpleError err) =
        return (err == failErrorMessage)

    checkFailAction = catch action failAction

processChunks'1 :: Property
processChunks'1 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            trBinary <- run ioTrBinary
            let
                inputStream = S.fromList ls

                genStrm = AS.concat $ S.rights $
                    Proc.processChunks'
                    trBinary
                    ["[a-z]", "[A-Z]"]
                    (AS.arraysOf arrayChunkElem inputStream)

                charUpperStrm = S.map toUpper inputStream

            genList <- run $ S.toList genStrm
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

processChunks'2 :: Property
processChunks'2 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            let
                inputStream = S.fromList ls
                outStream = AS.concat $ S.lefts $
                    Proc.processChunks'
                    executableFile
                    ["[a-z]", "[A-Z]"]
                    (AS.arraysOf arrayChunkElem inputStream)

                charUpperStrm = S.map toUpper inputStream

            genList <- run $ S.toList outStream
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

processChunks'3 :: Property
processChunks'3 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $ Proc.processChunks' executableFileFail [] S.nil
        return False

    failAction (ProcessFailure exitCode) =
        return (exitCode == 1)

    checkFailAction = catch action failAction

processChunks'4 :: Property
processChunks'4 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $
            Proc.processChunks'
            executableFilePass
            []
            (S.nilM $ throwM (SimpleError failErrorMessage))
        return False

    failAction (SimpleError err) =
        return (err == failErrorMessage)

    checkFailAction = catch action failAction

main :: IO ()
main = do
    createExecutables
    hspec $ do
        describe "test for process functions" $ do
            prop "toBytes' cat = FH.toBytes" toBytes1
            prop "toBytes' on failing executable" toBytes2
            prop "toChunks' cat = FH.toChunks" toChunks1
            prop "toChunks' on failing executable" toChunks2
            prop "processBytes tr = map toUpper" processBytes1
            prop "processBytes on failing executable" processBytes2
            prop "processBytes using error stream" processBytes3
            prop
                "AS.concat $ processChunks tr = map toUpper"
                processChunks1
            prop "processChunks on failing executable" processChunks2
            prop "processChunks using error stream" processChunks3
            prop "processBytes' tr = map toUpper" processBytes'1
            prop
                "error stream of processBytes' tr = map toUpper"
                processBytes'2
            prop "processBytes' on failing executable" processBytes'3
            prop "processBytes' using error stream" processBytes'4
            prop
                "AS.concat $ processChunks' tr = map toUpper"
                processChunks'1
            prop
                "error stream of processChunks' tr = map toUpper"
                processChunks'2
            prop "processChunks' on failing executable" processChunks'3
            prop "processChunks' using error stream" processChunks'4
    removeExecutables
