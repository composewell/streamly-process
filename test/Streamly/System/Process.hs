{-# LANGUAGE CPP #-}

module Main where

import Data.Function ((&))
import Data.List ((\\))
import Data.Maybe (fromMaybe)
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

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Prelude as S
import qualified Streamly.System.Process as Proc

-- Internal imports
import qualified Streamly.Internal.Data.Array.Stream.Foreign
    as AS (arraysOf, concat)
import qualified Streamly.Internal.Data.Stream.IsStream as S (nilM, lefts, rights)
import qualified Streamly.Internal.FileSystem.Handle as FH (putBytes, toBytes)
import qualified Streamly.Internal.System.Process as Proc

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

which :: String -> IO FilePath
which cmd = do
    r <- findExecutable cmd
    case r of
        Just path -> return path
        _ -> error $ "Required command " ++ cmd ++ " not found"

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

arrayChunkSize :: Int
arrayChunkSize = 100

-- XXX Commit these files to repo
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
            catBinary <- run $ which "cat"
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
            catBinary <- run $ which "cat"
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
            trBinary <- run $ which "tr"
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

-- Termination on input termination
processChunksConsumeAllInput :: Property
processChunksConsumeAllInput =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            trBinary <- run $ which "tr"
            let
                inputStream = S.fromList ls

                genStrm = AS.concat $
                    Proc.processChunks
                    trBinary
                    ["[a-z]", "[A-Z]"]
                    (AS.arraysOf arrayChunkSize inputStream)

                charUpperStrm = S.map toUpper inputStream

            genList <- run $ S.toList genStrm
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

processChunksConsumePartialInput :: Property
processChunksConsumePartialInput =
    forAll (listOf (choose(9, _z))) $ \ls ->
        monadicIO $ do
            path <- run $ which "head"
            let
                inputStream = S.fromList ls

                procOutput = AS.concat $
                    Proc.processChunks
                    path
                    ["-n", "1"]
                    (AS.arraysOf arrayChunkSize inputStream)

                headLine =
                      S.splitWithSuffix (== 10) Fold.toList inputStream
                    & S.head

            procList <- run $ S.toList procOutput
            expectedList <- run $ fmap (fromMaybe []) headLine
            listEquals (==) procList expectedList

processChunksProcessFailure :: Property
processChunksProcessFailure = monadicIO $ run $ catch runProcess checkExitCode

    where

    runProcess = do
        S.drain $ Proc.processChunks executableFileFail [] S.nil
        return False

    checkExitCode (ProcessFailure exitCode) = return (exitCode == 1)

processChunksInputFailure :: Property
processChunksInputFailure = monadicIO $ run $ catch runProcess checkException

    where

    runProcess = do
        S.drain $
            Proc.processChunks
            executableFilePass
            []
            (S.nilM $ throwM (SimpleError failErrorMessage))
        return False

    checkException (SimpleError err) = return (err == failErrorMessage)

processBytes'1 :: Property
processBytes'1 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            trBinary <- run $ which "tr"
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
            trBinary <- run $ which "tr"
            let
                inputStream = S.fromList ls

                genStrm = AS.concat $ S.rights $
                    Proc.processChunks'
                    trBinary
                    ["[a-z]", "[A-Z]"]
                    (AS.arraysOf arrayChunkSize inputStream)

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
                    (AS.arraysOf arrayChunkSize inputStream)

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
        describe "Streamly.System.Process" $ do
            -- XXX Add a test for garbage collection case. Also check whether
            -- the process is really gone after exception or gc.
            --
            -- Keep the tests in dependency order so that we test the basic
            -- things first.
            describe "processChunks'" $ do
                prop
                    "AS.concat $ processChunks' tr = map toUpper"
                    processChunks'1
                prop
                    "error stream of processChunks' tr = map toUpper"
                    processChunks'2
                prop "processChunks' on failing executable" processChunks'3
                prop "processChunks' using error stream" processChunks'4

            describe "processChunks" $ do
                prop "consumeAllInput" processChunksConsumeAllInput
                prop "consumePartialInput" processChunksConsumePartialInput
                prop "ProcessFailure" processChunksProcessFailure
                prop "inputFailure" processChunksInputFailure

            -- based on processChunks
            describe "processBytes'" $ do
                prop "processBytes' tr = map toUpper" processBytes'1
                prop
                    "error stream of processBytes' tr = map toUpper"
                    processBytes'2
                prop "processBytes' on failing executable" processBytes'3
                prop "processBytes' using error stream" processBytes'4

            describe "processBytes" $ do
                prop "processBytes tr = map toUpper" processBytes1
                prop "processBytes on failing executable" processBytes2
                prop "processBytes using error stream" processBytes3

            -- Based on processBytes/Chunks
            describe "toChunks'" $ do
                prop "toChunks' cat = FH.toChunks" toChunks1
                prop "toChunks' on failing executable" toChunks2

            describe "toBytes'" $ do
                prop "toBytes' cat = FH.toBytes" toBytes1
                prop "toBytes' on failing executable" toBytes2

    removeExecutables
