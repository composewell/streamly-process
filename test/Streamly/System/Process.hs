{-# LANGUAGE CPP #-}

module Main (main) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Data.Function ((&))
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Control.Exception (Exception)
import Control.Monad (unless)
import Control.Monad.Catch (throwM, catch)
import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Data.Array (Array, Unbox)
import Streamly.Data.Stream (Stream)
import Streamly.System.Process (ProcessFailure (..))
import System.Directory (removeFile, findExecutable, doesFileExist)
import System.Exit (exitSuccess)
import System.IO (IOMode(..), openFile, hClose)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.Hspec.QuickCheck
import Test.QuickCheck
    ( forAll
    , choose
    , Property
    , listOf
    , counterexample
    )
import Test.QuickCheck.Monadic (monadicIO, PropertyM, assert, monitor, run)

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as S
import qualified Streamly.System.Process as Proc
import qualified Streamly.Data.Stream as Stream

import qualified Streamly.Internal.FileSystem.Handle as FH (putBytes, read)
import qualified Streamly.Internal.System.Command as Cmd (quotedWord)

-------------------------------------------------------------------------------
-- Compatibility
-------------------------------------------------------------------------------

#if MIN_VERSION_streamly_core(0,3,0)
#define UNFOLD_EACH Stream.unfoldEach
#define CHUNKS_OF Array.chunksOf
#else
#define UNFOLD_EACH Stream.unfoldMany
#define CHUNKS_OF Stream.chunksOf
#endif

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

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

arrayChunkSize :: Int
arrayChunkSize = 100

interpreterFile :: FilePath
interpreterArg :: String
#ifdef mingw32_HOST_OS
interpreterFile = "cmd.exe"
interpreterArg = "/c"
#else
interpreterFile = "/usr/bin/env"
interpreterArg = "sh"
#endif

executableFile :: FilePath
#ifdef mingw32_HOST_OS
executableFile = "./test/data/writeTrToError.bat"
#else
executableFile = "./test/data/writeTrToError.sh"
#endif

executableFileFail :: FilePath
#ifdef mingw32_HOST_OS
executableFileFail = "./test/data/failExec.bat"
#else
executableFileFail = "./test/data/failExec.sh"
#endif

executableFilePass :: FilePath
#ifdef mingw32_HOST_OS
executableFilePass = "./test/data/passExec.bat"
#else
executableFilePass = "./test/data/passExec.sh"
#endif

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
                genStrm = S.catRights $ Proc.toBytesEither catBinary [charFile]

                ioByteStrm = do
                    handle <- openFile charFile ReadMode
                    let strm = FH.read handle
                    return $ S.finallyIO (hClose handle) strm

            run $ generateCharFile numBlock
            byteStrm <- run ioByteStrm
            genList <- run $ S.fold Fold.toList genStrm
            byteList <- run $ S.fold Fold.toList byteStrm
            run $ removeFile charFile
            listEquals (==) genList byteList

toBytes2 :: Property
toBytes2 = monadicIO $ run checkFailAction
    where

    action = do
        S.fold Fold.drain $
             Proc.toBytesEither interpreterFile [interpreterArg, executableFileFail]
        return False

    failAction (ProcessFailure exitCode) =
        return (exitCode == 1)

    checkFailAction = catch action failAction

{-# INLINE concatArr #-}
concatArr :: (Monad m, Unbox a) => Stream m (Array a) -> Stream m a
concatArr = UNFOLD_EACH Array.reader

toChunks1 :: Property
toChunks1 =
    forAll (choose (minBlockCount, maxBlockCount)) $ \numBlock ->
        monadicIO $ do
            catBinary <- run $ which "cat"
            let
                genStrm = S.catRights $ Proc.toChunksEither catBinary [charFile]

                ioByteStrm = do
                    handle <- openFile charFile ReadMode
                    let strm = FH.read handle
                    return $ S.finallyIO (hClose handle) strm

            run $ generateCharFile numBlock
            byteStrm <- run ioByteStrm
            genList <- run $ S.fold Fold.toList (concatArr genStrm)
            byteList <- run $ S.fold Fold.toList byteStrm
            run $ removeFile charFile
            listEquals (==) genList byteList

toChunks2 :: Property
toChunks2 = monadicIO $ run checkFailAction
    where

    action = do
        S.fold Fold.drain $
             Proc.toChunksEither interpreterFile [interpreterArg, executableFileFail]
        return False

    failAction (ProcessFailure exitCode) =
        return (exitCode == 1)

    checkFailAction = catch action failAction

pipeBytes1 :: Property
pipeBytes1 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            trBinary <- run $ which "tr"
            let
                inputStream = S.fromList ls
                genStrm = Proc.pipeBytes
                            trBinary
                            ["[a-z]", "[A-Z]"]
                            inputStream
                charUpperStrm = fmap toUpper inputStream

            genList <- run $ S.fold Fold.toList genStrm
            charList <- run $ S.fold Fold.toList charUpperStrm
            listEquals (==) genList charList

pipeBytes2 :: Property
pipeBytes2 = monadicIO $ run checkFailAction
    where

    action = do
        S.fold Fold.drain $
           Proc.pipeBytes
               interpreterFile [interpreterArg, executableFileFail] S.nil
        return False

    failAction (ProcessFailure exitCode) =
        return (exitCode == 1)

    checkFailAction = catch action failAction

pipeBytes3 :: Property
pipeBytes3 = monadicIO $ run checkFailAction
    where

    action = do
        S.fold Fold.drain $
            Proc.pipeBytes
            interpreterFile
            [interpreterArg, executableFilePass]
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

                genStrm = concatArr $
                    Proc.pipeChunks
                    trBinary
                    ["[a-z]", "[A-Z]"]
                    (CHUNKS_OF arrayChunkSize inputStream)

                charUpperStrm = fmap toUpper inputStream

            genList <- run $ S.fold Fold.toList genStrm
            charList <- run $ S.fold Fold.toList charUpperStrm
            listEquals (==) genList charList

processChunksConsumePartialInput :: Property
processChunksConsumePartialInput =
    forAll (listOf (choose(9, _z))) $ \ls ->
        monadicIO $ do
            path <- run $ which "head"
            let
                inputStream = S.fromList ls

                procOutput = concatArr $
                    Proc.pipeChunks
                    path
                    ["-n", "1"]
                    (CHUNKS_OF arrayChunkSize inputStream)

                headLine =
                      S.foldMany
                          (Fold.takeEndBy (== 10) Fold.toList)
                          inputStream
                    & S.fold Fold.one

            procList <- run $ S.fold Fold.toList procOutput
            expectedList <- run $ fmap (fromMaybe []) headLine
            listEquals (==) procList expectedList

processChunksProcessFailure :: Property
processChunksProcessFailure = monadicIO $ run $ catch runProcess checkExitCode

    where

    runProcess = do
        S.fold Fold.drain $
            Proc.pipeChunks
                interpreterFile [interpreterArg, executableFileFail] S.nil
        return False

    checkExitCode (ProcessFailure exitCode) = return (exitCode == 1)

processChunksInputFailure :: Property
processChunksInputFailure = monadicIO $ run $ catch runProcess checkException

    where

    runProcess = do
        S.fold Fold.drain $
            Proc.pipeChunks
            interpreterFile
            [interpreterArg, executableFilePass]
            (S.nilM $ throwM (SimpleError failErrorMessage))
        return False

    checkException (SimpleError err) = return (err == failErrorMessage)

pipeBytesEither1 :: Property
pipeBytesEither1 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            trBinary <- run $ which "tr"
            let
                inputStream = S.fromList ls
                genStrm = S.catRights $ Proc.pipeBytesEither
                            trBinary
                            ["[a-z]", "[A-Z]"]
                            inputStream
                charUpperStrm = fmap toUpper inputStream

            genList <- run $ S.fold Fold.toList genStrm
            charList <- run $ S.fold Fold.toList charUpperStrm
            listEquals (==) genList charList

pipeBytesEither2 :: Property
pipeBytesEither2 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            let
                inputStream = S.fromList ls
                outStream = S.catLefts $
                    Proc.pipeBytesEither
                    interpreterFile
                    [interpreterArg, executableFile, "[a-z]", "[A-Z]"]
                    inputStream

                charUpperStrm = fmap toUpper inputStream

            genList <- run $ S.fold Fold.toList outStream
            charList <- run $ S.fold Fold.toList charUpperStrm
            listEquals (==) genList charList

pipeBytesEither3 :: Property
pipeBytesEither3 = monadicIO $ run checkFailAction
    where

    action = do
        S.fold Fold.drain $
            Proc.pipeBytesEither
                interpreterFile [interpreterArg, executableFileFail] S.nil
        return False

    failAction (ProcessFailure exitCode) =
        return (exitCode == 1)

    checkFailAction = catch action failAction

pipeBytesEither4 :: Property
pipeBytesEither4 = monadicIO $ run checkFailAction
    where

    action = do
        S.fold Fold.drain $
            Proc.pipeBytesEither
            interpreterFile
            [interpreterArg, executableFilePass]
            (S.nilM $ throwM (SimpleError failErrorMessage))
        return False

    failAction (SimpleError err) =
        return (err == failErrorMessage)

    checkFailAction = catch action failAction

pipeChunksEither1 :: Property
pipeChunksEither1 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            trBinary <- run $ which "tr"
            let
                inputStream = S.fromList ls

                genStrm = concatArr $ S.catRights $
                    Proc.pipeChunksEither
                    trBinary
                    ["[a-z]", "[A-Z]"]
                    (CHUNKS_OF arrayChunkSize inputStream)

                charUpperStrm = fmap toUpper inputStream

            genList <- run $ S.fold Fold.toList genStrm
            charList <- run $ S.fold Fold.toList charUpperStrm
            listEquals (==) genList charList

pipeChunksEither2 :: Property
pipeChunksEither2 =
    forAll (listOf (choose(_a, _z))) $ \ls ->
        monadicIO $ do
            let
                inputStream = S.fromList ls
                outStream = concatArr $ S.catLefts $
                    Proc.pipeChunksEither
                    interpreterFile
                    [interpreterArg, executableFile, "[a-z]", "[A-Z]"]
                    (CHUNKS_OF arrayChunkSize inputStream)

                charUpperStrm = fmap toUpper inputStream

            genList <- run $ S.fold Fold.toList outStream
            charList <- run $ S.fold Fold.toList charUpperStrm
            listEquals (==) genList charList

pipeChunksEither3 :: Property
pipeChunksEither3 = monadicIO $ run checkFailAction
    where

    action = do
        S.fold Fold.drain $ Proc.pipeChunksEither
            interpreterFile  [interpreterArg, executableFileFail] S.nil
        return False

    failAction (ProcessFailure exitCode) =
        return (exitCode == 1)

    checkFailAction = catch action failAction

pipeChunksEither4 :: Property
pipeChunksEither4 = monadicIO $ run checkFailAction
    where

    action = do
        S.fold Fold.drain $
            Proc.pipeChunksEither
            interpreterFile
            [interpreterArg, executableFilePass]
            (S.nilM $ throwM (SimpleError failErrorMessage))
        return False

    failAction (SimpleError err) =
        return (err == failErrorMessage)

    checkFailAction = catch action failAction

quotedWordTest :: String -> [String] -> IO ()
quotedWordTest inp expected = do
    res <-
        Stream.fold Fold.toList
            $ Stream.catRights
            $ Stream.parseMany Cmd.quotedWord $ Stream.fromList inp
    res `shouldBe` expected

main :: IO ()
main = do
    -- Nix does not have "/usr/bin/env", so the execution of test executables
    -- fails. We can create the executables as temporary files to avoid this
    -- issue. Even creating Haskell executables does not work because of
    -- https://github.com/haskell/cabal/issues/7577 .
    r <- doesFileExist "/usr/bin/env"
    unless r $ do
        putStrLn "/usr/bin/env does not exist, skipping tests."
        exitSuccess

    hspec $ do
        describe "Streamly.System.Process" $ do
            -- XXX Add a test for garbage collection case. Also check whether
            -- the process is really gone after exception or gc.
            --
            -- Keep the tests in dependency order so that we test the basic
            -- things first.
            describe "pipeChunksEither" $ do
                prop
                    "concatArr $ pipeChunksEither tr = map toUpper"
                    pipeChunksEither1
                prop
                    "error stream of pipeChunksEither tr = map toUpper"
                    pipeChunksEither2
                prop "pipeChunksEither on failing executable" pipeChunksEither3
                prop "pipeChunksEither using error stream" pipeChunksEither4

            describe "processChunks" $ do
                prop "consumeAllInput" processChunksConsumeAllInput
                prop "consumePartialInput" processChunksConsumePartialInput
                prop "ProcessFailure" processChunksProcessFailure
                prop "inputFailure" processChunksInputFailure

            -- based on processChunks
            describe "pipeBytesEither" $ do
                prop "pipeBytesEither tr = map toUpper" pipeBytesEither1
                prop
                    "error stream of pipeBytesEither tr = map toUpper"
                    pipeBytesEither2
                prop "pipeBytesEither on failing executable" pipeBytesEither3
                prop "pipeBytesEither using error stream" pipeBytesEither4

            describe "pipeBytes" $ do
                prop "pipeBytes tr = map toUpper" pipeBytes1
                prop "pipeBytes on failing executable" pipeBytes2
                prop "pipeBytes using error stream" pipeBytes3

            -- Based on pipeBytes/Chunks
            describe "toChunksEither" $ do
                prop "toChunksEither cat = FH.toChunks" toChunks1
                prop "toChunksEither on failing executable" toChunks2

            describe "toBytesEither" $ do
                prop "toBytesEither cat = FH.toBytes" toBytes1
                prop "toBytesEither on failing executable" toBytes2

            describe "quotedWord" $ do
                it "Single quote test" $
                   quotedWordTest "'hello\\\\\"world'" ["hello\\\\\"world"]
                it "Double quote test" $
                   quotedWordTest
                       "\"hello\\\"\\\\w\\'orld\""
                       ["hello\"\\w\\'orld"]
                -- TODO: We need to let the escape character be at the end
                -- "wordWithQuotes" needs to be fixed!
                -- it "Double quote test" $
                --    quotedWordTest "'hello\'" ["hello\\"]
