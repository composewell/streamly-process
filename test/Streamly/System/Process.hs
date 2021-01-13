module Main where

import qualified Streamly.Internal.Prelude as S
import qualified Streamly.System.Process as Proc
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Memory.ArrayStream as AS
import Streamly.System.Process (ProcessFailed (..))
import Streamly.Internal.Data.Fold (Fold)

import System.IO (IOMode(..), openFile, hClose)
import System.Process (callCommand)
import System.Directory (removeFile, findExecutable)

import Test.Hspec(hspec, describe)
import Test.Hspec.QuickCheck
import Test.QuickCheck
    ( forAll
    , choose
    , Property

    , listOf

    , counterexample

    )

import Test.QuickCheck.Monadic (monadicIO, PropertyM, assert, monitor, run)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (unless)
import Control.Monad.Catch (throwM, catch)
import Control.Exception (Exception)

import Data.IORef (IORef (..), newIORef, readIORef, writeIORef)
import Data.List ((\\))
import Data.Word (Word8)

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
executableFile = "./writeTrToError.sh"

executableFileContent :: String
executableFileContent =
    "tr [a-z] [A-Z] <&0 >&2"

executableFileFail :: FilePath
executableFileFail = "./failExec.sh"

executableFileFailContent :: String
executableFileFailContent =
    "exit 1"

executableFilePass :: FilePath
executableFilePass = "./passExec.sh"

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
    FH.fromBytes handle (S.replicate numCharInCharFile _a)
    hClose handle

writeToIoRefFold ::
    MonadIO m
    => IORef [Word8]
    -> Fold m Word8 ()
writeToIoRefFold ioRef = FL.Fold step initial extract

    where

    step _ newEle = do
        list <- liftIO $ readIORef ioRef
        let newList = newEle : list
        liftIO $ writeIORef ioRef newList
        return ()

    initial = liftIO $ writeIORef ioRef []

    extract _ = do
        list <- liftIO $ readIORef ioRef
        let reverseList = Prelude.reverse list
        liftIO $ writeIORef ioRef reverseList

toBytes1 :: Property
toBytes1 =
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

toBytes2 :: Property
toBytes2 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $ Proc.toBytes executableFileFail []
        return False

    failAction (ProcessFailed exitCode) =
        return (exitCode == 1)

    checkFailAction = catch action failAction

toChunks1 :: Property
toChunks1 =
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

toChunks2 :: Property
toChunks2 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $ Proc.toChunks executableFileFail []
        return False

    failAction (ProcessFailed exitCode) =
        return (exitCode == 1)

    checkFailAction = catch action failAction

transformBytes_1 :: Property
transformBytes_1 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            trBinary <- run ioTrBinary
            let
                inputStream = S.fromList ls
                genStrm = Proc.transformBytes_
                            trBinary
                            ["[a-z]", "[A-Z]"]
                            inputStream
                charUpperStrm = S.map toUpper inputStream

            genList <- run $ S.toList genStrm
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

transformBytes_2 :: Property
transformBytes_2 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $ Proc.transformBytes_ executableFileFail [] S.nil
        return False

    failAction (ProcessFailed exitCode) =
        return (exitCode == 1)

    checkFailAction = catch action failAction

transformBytes_3 :: Property
transformBytes_3 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $
            Proc.transformBytes_
            executableFilePass
            []
            (S.nilM $ throwM (SimpleError failErrorMessage))
        return False

    failAction (SimpleError err) =
        return (err == failErrorMessage)

    checkFailAction = catch action failAction

transformChunks_1 :: Property
transformChunks_1 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            trBinary <- run ioTrBinary
            let
                inputStream = S.fromList ls

                genStrm = AS.concat $
                    Proc.transformChunks_
                    trBinary
                    ["[a-z]", "[A-Z]"]
                    (AS.arraysOf arrayChunkElem inputStream)

                charUpperStrm = S.map toUpper inputStream

            genList <- run $ S.toList genStrm
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

transformChunks_2 :: Property
transformChunks_2 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $ Proc.transformChunks_ executableFileFail [] S.nil
        return False

    failAction (ProcessFailed exitCode) =
        return (exitCode == 1)

    checkFailAction = catch action failAction

transformChunks_3 :: Property
transformChunks_3 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $
            Proc.transformChunks_
            executableFilePass
            []
            (S.nilM $ throwM (SimpleError failErrorMessage))
        return False

    failAction (SimpleError err) =
        return (err == failErrorMessage)

    checkFailAction = catch action failAction

transformBytes1 :: Property
transformBytes1 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            trBinary <- run ioTrBinary
            let
                inputStream = S.fromList ls
                genStrm = Proc.transformBytes
                            trBinary
                            ["[a-z]", "[A-Z]"]
                            FL.drain inputStream
                charUpperStrm = S.map toUpper inputStream

            genList <- run $ S.toList genStrm
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

transformBytes2 :: Property
transformBytes2 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            listIoRef <- run $ newIORef []
            let
                inputStream = S.fromList ls
                outStream =
                    Proc.transformBytes
                    executableFile
                    ["[a-z]", "[A-Z]"]
                    (writeToIoRefFold listIoRef)
                    inputStream

                charUpperStrm = S.map toUpper inputStream

            run $ S.drain outStream
            genList <- run $ readIORef listIoRef
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

transformBytes3 :: Property
transformBytes3 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $ Proc.transformBytes executableFileFail [] FL.drain S.nil
        return False

    failAction (ProcessFailed exitCode) =
        return (exitCode == 1)

    checkFailAction = catch action failAction

transformBytes4 :: Property
transformBytes4 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $
            Proc.transformBytes
            executableFilePass
            []
            FL.drain
            (S.nilM $ throwM (SimpleError failErrorMessage))
        return False

    failAction (SimpleError err) =
        return (err == failErrorMessage)

    checkFailAction = catch action failAction

transformChunks1 :: Property
transformChunks1 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            trBinary <- run ioTrBinary
            let
                inputStream = S.fromList ls

                genStrm = AS.concat $
                    Proc.transformChunks
                    trBinary
                    ["[a-z]", "[A-Z]"]
                    FL.drain
                    (AS.arraysOf arrayChunkElem inputStream)

                charUpperStrm = S.map toUpper inputStream

            genList <- run $ S.toList genStrm
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

transformChunks2 :: Property
transformChunks2 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            listIoRef <- run $ newIORef []
            let
                inputStream = S.fromList ls
                outStream =
                    Proc.transformChunks
                    executableFile
                    ["[a-z]", "[A-Z]"]
                    (writeToIoRefFold listIoRef)
                    (AS.arraysOf arrayChunkElem inputStream)

                charUpperStrm = S.map toUpper inputStream

            run $ S.drain outStream
            genList <- run $ readIORef listIoRef
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

transformChunks3 :: Property
transformChunks3 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $ Proc.transformChunks executableFileFail [] FL.drain S.nil
        return False

    failAction (ProcessFailed exitCode) =
        return (exitCode == 1)

    checkFailAction = catch action failAction

transformChunks4 :: Property
transformChunks4 = monadicIO $ run checkFailAction
    where

    action = do
        S.drain $
            Proc.transformChunks
            executableFilePass
            []
            FL.drain
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
            prop "toBytes cat = FH.toBytes" toBytes1
            prop "toBytes on failing executable" toBytes2
            prop "toChunks cat = FH.toChunks" toChunks1
            prop "toChunks on failing executable" toChunks2
            prop "transformBytes_ tr = map toUpper" transformBytes_1
            prop "transformBytes_ on failing executable" transformBytes_2
            prop "transformBytes_ using error stream" transformBytes_3
            prop
                "AS.concat $ transformChunks_ tr = map toUpper"
                transformChunks_1
            prop "transformChunks_ on failing executable" transformChunks_2
            prop "transformChunks_ using error stream" transformChunks_3
            prop "transformBytes tr = map toUpper" transformBytes1
            prop
                "error stream of transformBytes tr = map toUpper"
                transformBytes2
            prop "transformBytes on failing executable" transformBytes3
            prop "transformBytes using error stream" transformBytes4
            prop
                "AS.concat $ transformChunks tr = map toUpper"
                transformChunks1
            prop
                "error stream of transformChunks tr = map toUpper"
                transformChunks2
            prop "transformChunks on failing executable" transformChunks3
            prop "transformChunks using error stream" transformChunks4
    removeExecutables
