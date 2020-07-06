module Main where

import qualified Streamly.Internal.Prelude as S
import qualified Streamly.System.Process as Proc
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Memory.ArrayStream as AS
import Streamly.System.Process (ProcessFailed (..))
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)
import Streamly.Internal.Data.Fold (Fold)

import System.IO (FilePath, Handle, IOMode(..), openFile, hClose, writeFile)
import System.Process (proc, createProcess, waitForProcess, callCommand)
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
import Control.Monad.Catch (catch)
import Control.Exception (ErrorCall(ErrorCallWithLocation))

import Data.IORef (IORef (..), newIORef, readIORef, writeIORef)
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

writeToIoRefFold :: 
    (IsStream t, MonadIO m)
    => IORef (t m Word8)
    -> Fold m Word8 ()
writeToIoRefFold ioRef = FL.Fold step initial extract

    where
    
    step _ newEle = do
        stream <- liftIO $ readIORef ioRef
        let newStream = S.cons newEle stream
        liftIO $ writeIORef ioRef newStream
        return ()

    initial = liftIO $ writeIORef ioRef S.nil

    extract _ = do
        stream <- liftIO $ readIORef ioRef
        let reverseStream = S.reverse stream
        liftIO $ writeIORef ioRef reverseStream 

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

transformBytes1 :: Property
transformBytes1 = 
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

transformBytes2 :: Property
transformBytes2 = monadicIO $ run checkFailAction
    where
    
    action = do
        S.drain $ Proc.transformBytes executableFileFail [] S.nil
        return False

    failAction (ProcessFailed exitCode) =
        return (exitCode == 1)
    
    checkFailAction = catch action failAction

transformBytes3 :: Property
transformBytes3 = monadicIO $ run checkFailAction
    where
    
    action = do
        S.drain $ 
            Proc.transformBytes
            executableFilePass 
            []
            (S.nilM $ error failErrorMessage)
        return False

    failAction (ErrorCallWithLocation err _) =
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
                    (AS.arraysOf arrayChunkElem inputStream)
                
                charUpperStrm = S.map toUpper inputStream

            genList <- run $ S.toList genStrm
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

transformChunks2 :: Property
transformChunks2 = monadicIO $ run checkFailAction
    where
    
    action = do
        S.drain $ Proc.transformChunks executableFileFail [] S.nil
        return False

    failAction (ProcessFailed exitCode) =
        return (exitCode == 1)
    
    checkFailAction = catch action failAction

transformChunks3 :: Property
transformChunks3 = monadicIO $ run checkFailAction
    where
    
    action = do
        S.drain $ 
            Proc.transformChunks
            executableFilePass 
            []
            (S.nilM $ error failErrorMessage)
        return False

    failAction (ErrorCallWithLocation err _) =
        return (err == failErrorMessage)
    
    checkFailAction = catch action failAction

thruExe1 :: Property
thruExe1 = 
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            trBinary <- run ioTrBinary
            let
                inputStream = S.fromList ls
                genStrm = Proc.thruExe trBinary ["[a-z]", "[A-Z]"] FL.drain inputStream
                charUpperStrm = S.map toUpper inputStream

            genList <- run $ S.toList genStrm
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

thruExe2 :: Property
thruExe2 = 
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            streamIoRef <- run $ newIORef S.nil
            let
                inputStream = S.fromList ls
                outStream = 
                    Proc.thruExe
                    executableFile
                    ["[a-z]", "[A-Z]"]
                    (writeToIoRefFold streamIoRef)
                    inputStream

                charUpperStrm = S.map toUpper inputStream

            run $ S.drain outStream
            genStrm <- run $ readIORef streamIoRef
            genList <- run $ S.toList genStrm
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

thruExe3 :: Property
thruExe3 = monadicIO $ run checkFailAction
    where
    
    action = do
        S.drain $ Proc.thruExe executableFileFail [] FL.drain S.nil
        return False

    failAction (ProcessFailed exitCode) =
        return (exitCode == 1)
    
    checkFailAction = catch action failAction

thruExe4 :: Property
thruExe4 = monadicIO $ run checkFailAction
    where
    
    action = do
        S.drain $ 
            Proc.thruExe
            executableFilePass 
            []
            FL.drain
            (S.nilM $ error failErrorMessage)
        return False

    failAction (ErrorCallWithLocation err _) =
        return (err == failErrorMessage)
    
    checkFailAction = catch action failAction

thruExeChunks1 :: Property
thruExeChunks1 = 
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

thruExeChunks2 :: Property
thruExeChunks2 = 
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            streamIoRef <- run $ newIORef S.nil
            let
                inputStream = S.fromList ls
                outStream = 
                    Proc.thruExeChunks
                    executableFile
                    ["[a-z]", "[A-Z]"]
                    (writeToIoRefFold streamIoRef)
                    (AS.arraysOf arrayChunkElem inputStream)

                charUpperStrm = S.map toUpper inputStream

            run $ S.drain outStream
            genStrm <- run $ readIORef streamIoRef
            genList <- run $ S.toList genStrm
            charList <- run $ S.toList charUpperStrm
            listEquals (==) genList charList

thruExeChunks3 :: Property
thruExeChunks3 = monadicIO $ run checkFailAction
    where
    
    action = do
        S.drain $ Proc.thruExeChunks executableFileFail [] FL.drain S.nil
        return False

    failAction (ProcessFailed exitCode) =
        return (exitCode == 1)
    
    checkFailAction = catch action failAction

thruExeChunks4 :: Property
thruExeChunks4 = monadicIO $ run checkFailAction
    where
    
    action = do
        S.drain $ 
            Proc.thruExeChunks
            executableFilePass 
            []
            FL.drain
            (S.nilM $ error failErrorMessage)
        return False

    failAction (ErrorCallWithLocation err _) =
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
            prop "transformBytes tr = map toUpper" transformBytes1
            prop "transformBytes on failing executable" transformBytes2
            prop "transformBytes using error stream" transformBytes3
            prop "AS.concat $ transformChunks tr = map toUpper" transformChunks1
            prop "transformChunks on failing executable" transformChunks2
            prop "transformChunks using error stream" transformChunks3
            prop "thruExe tr = map toUpper" thruExe1
            prop "error stream of thruExe tr = map toUpper" thruExe2
            prop "thruExe on failing executable" thruExe3
            prop "thruExe using error stream" thruExe4
            prop "AS.concat $ thruExeChunks tr = map toUpper" thruExeChunks1
            prop "error stream of thruExeChunks tr = map toUpper" thruExeChunks2
            prop "thruExeChunks on failing executable" thruExeChunks3
            prop "thruExeChunks using error stream" thruExeChunks4
    removeExecutables