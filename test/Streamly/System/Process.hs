{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Data.Bits ((.&.))
import Data.Function ((&))
import Data.List ((\\), isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Control.Concurrent (threadDelay)
import Control.Exception
    ( AsyncException(..)
    , Exception
    , IOException
    , displayException
    , finally
    , try
    )
import Numeric (readHex)
import GHC.IO.Exception (IOErrorType(..))
import System.Environment (setEnv)
import System.IO.Error (ioeGetErrorType, isDoesNotExistError)
import Control.Monad (unless)
import Control.Monad.Catch (throwM, catch)
import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Data.Array (Array, Unbox)
import Streamly.Data.Stream (Stream)
import Streamly.System.Process (ProcessFailure (..))
import System.Directory
    ( canonicalizePath
    , doesFileExist
    , findExecutable
    , getCurrentDirectory
    , removeFile
    )
import System.Exit (ExitCode(..), exitSuccess)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (IOMode(..), hClose, hFlush, openFile, stdout)
#if !defined(mingw32_HOST_OS)
import System.Posix.IO (closeFd, createPipe)
#endif
import System.Process (getProcessExitCode, terminateProcess, waitForProcess)
import Test.Hspec
    (describe, expectationFailure, hspec, it, shouldBe, shouldSatisfy)
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
import qualified Streamly.System.Command as Command
import qualified Streamly.System.Process as Proc
import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.Data.Stream as Stream

import qualified Streamly.Internal.FileSystem.Handle as FH (putBytes, read)
import qualified Streamly.Internal.System.Process as IProc
import qualified Streamly.Internal.System.Command as Cmd

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

-------------------------------------------------------------------------------
-- Processes that do not read all of the input
-------------------------------------------------------------------------------

-- | More bytes than a pipe buffer holds, so that a writer blocks unless the
-- process reads it.
largeInputSize :: Int
largeInputSize = 1024 * 1024

largeInput :: Stream IO Word8
largeInput = S.replicate largeInputSize _a

largeInputChunks :: Stream IO (Array Word8)
largeInputChunks = CHUNKS_OF arrayChunkSize largeInput

-- | @head -c 1@ exits after reading one byte; the rest of the input must be
-- discarded without an exception, like in a shell pipeline.
pipeBytesPartialInput :: IO ()
pipeBytesPartialInput = do
    r <- S.fold Fold.toList $ Proc.pipeBytes "head" ["-c", "1"] largeInput
    r `shouldBe` [_a]

pipeBytesInfiniteInput :: IO ()
pipeBytesInfiniteInput = do
    r <- S.fold Fold.toList $ Proc.pipeBytes "head" ["-c", "1"] (S.repeat _a)
    r `shouldBe` [_a]

pipeChunksNoInputRead :: IO ()
pipeChunksNoInputRead = do
    r <- S.fold Fold.toList
        $ concatArr
        $ Proc.pipeChunks "true" [] largeInputChunks
    r `shouldBe` []

pipeChunksEitherStdinClosed :: IO ()
pipeChunksEitherStdinClosed = do
    r <- S.fold Fold.toList
        $ concatArr
        $ S.catRights
        $ Proc.pipeChunksEither
            "sh" ["-c", "exec 0<&-; echo hi"] largeInputChunks
    r `shouldBe` [104, 105, 10]

pipeBytesEitherPartialInput :: IO ()
pipeBytesEitherPartialInput = do
    r <- S.fold Fold.toList
        $ S.catRights
        $ Proc.pipeBytesEither "head" ["-c", "1"] largeInput
    r `shouldBe` [_a]

-- | When the process fails without reading the input, the failure must be
-- reported as 'ProcessFailure', not as a write error on the pipe.
pipeChunksNoInputReadFailure :: IO ()
pipeChunksNoInputReadFailure = do
    r <- (S.fold Fold.drain
            (Proc.pipeChunks "sh" ["-c", "exit 2"] largeInputChunks)
            >> return Nothing)
        `catch` (\(ProcessFailure code) -> return (Just code))
    r `shouldBe` Just 2

-------------------------------------------------------------------------------
-- Inheriting stdout in the Either APIs
-------------------------------------------------------------------------------

-- | With stdout inherited only the stderr stream is produced. The process
-- writes nothing to stdout to keep the test output clean.
pipeChunksEitherInheritStdout :: IO ()
pipeChunksEitherInheritStdout = do
    r <- S.fold Fold.toList
        $ fmap (either (Left . Array.toList) (Right . Array.toList))
        $ IProc.pipeChunksEitherWith
            IProc.inheritStdout "sh" ["-c", "cat >&2"]
            (S.fromPure (Array.fromList [104, 105]))
    r `shouldBe` [Left [104, 105]]

toChunksEitherInheritStdout :: IO ()
toChunksEitherInheritStdout = do
    r <- S.fold Fold.toList
        $ fmap (either (Left . Array.toList) (Right . Array.toList))
        $ IProc.toChunksEitherWith
            IProc.inheritStdout "sh" ["-c", "printf hi >&2"]
    r `shouldBe` [Left [104, 105]]

-------------------------------------------------------------------------------
-- Config modifiers
-------------------------------------------------------------------------------

-- | Run a command with a config modifier and return its stdout as a string.
toStringWith ::
    (Proc.Config -> Proc.Config) -> FilePath -> [String] -> IO String
toStringWith modCfg path args =
    S.fold Fold.toList
        $ Unicode.decodeUtf8
        $ concatArr
        $ Proc.toChunksWith modCfg path args

-- | The pid and the process group id of a process run with the modifier.
pidAndPgid :: (Proc.Config -> Proc.Config) -> IO (Int, Int)
pidAndPgid modCfg = do
    out <- toStringWith modCfg "sh" ["-c", "echo $$; ps -o pgid= -p $$"]
    case map read (words out) of
        [pid, pgid] -> return (pid, pgid)
        _ -> error $ "pidAndPgid: unexpected output: " ++ show out

-- | A process is a group leader if its pgid is its pid. A new session makes
-- the process a group leader, an inherited session does not.
isGroupLeader :: (Proc.Config -> Proc.Config) -> IO Bool
isGroupLeader modCfg = uncurry (==) <$> pidAndPgid modCfg

setSessionLastWins :: IO ()
setSessionLastWins = do
    r <- isGroupLeader
            (Proc.setSession Proc.InheritSession
                . Proc.setSession Proc.NewSession)
    r `shouldBe` False

-------------------------------------------------------------------------------
-- Standalone processes
-------------------------------------------------------------------------------

-- | Time given to a process to die if it was wrongly terminated.
settleTime :: Int
settleTime = 300000

-- | @daemon@ and @standalone False@ must return without terminating the
-- process.
daemonKeepsRunning :: IO ()
daemonKeepsRunning = do
    ph <- Proc.daemon id "sleep" ["10"]
    threadDelay settleTime
    st <- getProcessExitCode ph
    terminateProcess ph
    _ <- waitForProcess ph
    st `shouldBe` Nothing

standaloneNoWaitKeepsRunning :: IO ()
standaloneNoWaitKeepsRunning = do
    r <- Proc.standalone False (True, True, True) id "sleep" ["10"]
    case r of
        Left code -> error $ "standalone returned exit code " ++ show code
        Right ph -> do
            threadDelay settleTime
            st <- getProcessExitCode ph
            terminateProcess ph
            _ <- waitForProcess ph
            st `shouldBe` Nothing

standaloneWaitExitCode :: IO ()
standaloneWaitExitCode = do
    r <- Proc.standalone True (True, True, True) id "sh" ["-c", "exit 3"]
    either Just (const Nothing) r `shouldBe` Just (ExitFailure 3)

-------------------------------------------------------------------------------
-- Invalid command strings
-------------------------------------------------------------------------------

-- | The action must raise an 'IOException' of type 'InvalidArgument'.
shouldBeInvalidArg :: Show a => IO a -> IO ()
shouldBeInvalidArg action = do
    r <- try action
    case r of
        Left (e :: IOException) ->
            (ioeGetErrorType e, e) `shouldSatisfy` ((== InvalidArgument) . fst)
        Right v ->
            expectationFailure
                $ "expected an InvalidArgument IOException, got " ++ show v

-- | Command strings that are empty or cannot be parsed.
invalidCommands :: [String]
invalidCommands =
    [ ""
    , "   "
    , "echo 'hello"
    , "echo \"hello"
    , "echo hello\\"
    ]

invalidCommandRunWith :: String -> IO ()
invalidCommandRunWith cmd = shouldBeInvalidArg (Cmd.toString cmd)

invalidCommandStreamWith :: String -> IO ()
invalidCommandStreamWith cmd =
    shouldBeInvalidArg (S.fold Fold.toList (Cmd.toBytes cmd))

invalidCommandPipeWith :: String -> IO ()
invalidCommandPipeWith cmd =
    shouldBeInvalidArg (S.fold Fold.toList (Cmd.pipeBytes cmd S.nil))

shellWordTest :: String -> [String] -> IO ()
shellWordTest inp expected = do
    res <-
        Stream.fold Fold.toList
            $ Stream.catRights
            $ Stream.parseMany Cmd.shellWord $ Stream.fromList inp
    res `shouldBe` expected

-------------------------------------------------------------------------------
-- Helpers for the API coverage tests
-------------------------------------------------------------------------------

ascii :: String -> [Word8]
ascii = map (fromIntegral . fromEnum)

-- | Arguments of @printf@ to print "hé" and a newline, é is two bytes in
-- UTF-8.
printfUtf8 :: [String]
printfUtf8 = ["h\\303\\251\\n"]

utf8Text :: String
utf8Text = "h\233\n"

-- | 'True' if the action raises 'ProcessFailure' with the given exit code.
failsWith :: Int -> IO a -> IO Bool
failsWith code action =
    (action >> return False)
        `catch` \(ProcessFailure c) -> return (c == code)

-- | Run an action with stdout redirected to a file, return the output.
captureStdout :: IO () -> IO String
captureStdout action = do
    let file = "./stdoutCapture"
    hFlush stdout
    saved <- hDuplicate stdout
    h <- openFile file WriteMode
    hDuplicateTo h stdout
    hClose h
    action `finally` (hFlush stdout >> hDuplicateTo saved stdout)
    hClose saved
    out <- readFile file
    length out `seq` removeFile file
    return out

eitherToLists ::
    Stream IO (Either (Array Word8) (Array Word8)) -> IO ([Word8], [Word8])
eitherToLists =
    S.fold (Fold.partition (Fold.lmap Array.toList Fold.mconcat)
                           (Fold.lmap Array.toList Fold.mconcat))

-------------------------------------------------------------------------------
-- Streamly.System.Process API coverage
-------------------------------------------------------------------------------

processFailureDisplay :: IO ()
processFailureDisplay =
    displayException (ProcessFailure 3)
        `shouldBe` "Process failed with exit code: 3"

-- | A process terminated by a signal fails with the negated signal number.
processFailureSignal :: IO ()
processFailureSignal =
    failsWith (-15) (Proc.toString "sh" ["-c", "kill -TERM $$"])
        >>= (`shouldBe` True)

execFailure :: IO ()
execFailure = do
    r <- try (Proc.toString "/nonexistent/executable" [])
    case r of
        Left (e :: IOException) -> e `shouldSatisfy` isDoesNotExistError
        Right v -> expectationFailure $ "expected an exception, got " ++ v

toChunksEcho :: IO ()
toChunksEcho =
    S.fold Fold.toList (concatArr (Proc.toChunks "echo" ["hello"]))
        >>= (`shouldBe` ascii "hello\n")

toBytesEcho :: IO ()
toBytesEcho =
    S.fold Fold.toList (Proc.toBytes "echo" ["hello"])
        >>= (`shouldBe` ascii "hello\n")

toBytesWithCwd :: IO ()
toBytesWithCwd =
    S.fold Fold.toList (Proc.toBytesWith (Proc.setCwd (Just "/")) "pwd" [])
        >>= (`shouldBe` ascii "/\n")

toCharsUtf8 :: IO ()
toCharsUtf8 =
    S.fold Fold.toList (Proc.toChars "printf" printfUtf8)
        >>= (`shouldBe` utf8Text)

-- | Invalid UTF-8 is decoded as the replacement character U+FFFD.
toCharsInvalidUtf8 :: IO ()
toCharsInvalidUtf8 =
    S.fold Fold.toList (Proc.toChars "printf" ["a\\377b"])
        >>= (`shouldBe` "a\65533b")

-- | The last line is emitted even without a trailing newline. A carriage
-- return before the newline is kept.
toLinesSplit :: IO ()
toLinesSplit = do
    S.fold Fold.toList (Proc.toLines Fold.toList "printf" ["a\\nb\\n\\nc"])
        >>= (`shouldBe` ["a", "b", "", "c"])
    S.fold Fold.toList (Proc.toLines Fold.toList "printf" ["a\\r\\nb"])
        >>= (`shouldBe` ["a\r", "b"])

toStringEcho :: IO ()
toStringEcho = Proc.toString "echo" ["hello"] >>= (`shouldBe` "hello\n")

toStringFailure :: IO ()
toStringFailure =
    failsWith 1 (Proc.toString "false" []) >>= (`shouldBe` True)

toStdoutEcho :: IO ()
toStdoutEcho =
    captureStdout (Proc.toStdout "echo" ["hello"]) >>= (`shouldBe` "hello\n")

toNullEcho :: IO ()
toNullEcho = Proc.toNull "echo" ["hello"] >>= (`shouldBe` ())

toNullFailure :: IO ()
toNullFailure = failsWith 1 (Proc.toNull "false" []) >>= (`shouldBe` True)

pipeChunksWithEnv :: IO ()
pipeChunksWithEnv = do
    r <- S.fold Fold.toList
        $ concatArr
        $ Proc.pipeChunksWith
            (Proc.setEnv (Just [("STREAMLY_PROCESS_TEST", "1")]))
            "/bin/sh" ["-c", "cat; echo $STREAMLY_PROCESS_TEST"]
            (S.fromPure (Array.fromList (ascii "a")))
    r `shouldBe` ascii "a1\n"

toChunksEitherWithCwd :: IO ()
toChunksEitherWithCwd = do
    r <- eitherToLists
        $ Proc.toChunksEitherWith (Proc.setCwd (Just "/"))
            "sh" ["-c", "pwd; printf x >&2"]
    r `shouldBe` (ascii "x", ascii "/\n")

pipeChunksEitherWithCwd :: IO ()
pipeChunksEitherWithCwd = do
    r <- eitherToLists
        $ Proc.pipeChunksEitherWith (Proc.setCwd (Just "/"))
            "sh" ["-c", "cat; pwd >&2"]
            (S.fromPure (Array.fromList (ascii "a")))
    r `shouldBe` (ascii "/\n", ascii "a")

foregroundExitCode :: IO ()
foregroundExitCode = do
    Proc.foreground id "sh" ["-c", "exit 3"] >>= (`shouldBe` ExitFailure 3)
    Proc.foreground id "true" [] >>= (`shouldBe` ExitSuccess)

-- | With interruptChildOnly, a child killed by SIGINT raises UserInterrupt.
foregroundSigInt :: IO ()
foregroundSigInt = do
    r <- try (Proc.foreground id "sh" ["-c", "kill -INT $$"])
    r `shouldBe` Left UserInterrupt
    r1 <- try
        $ S.fold Fold.drain
        $ Proc.toChunksWith (Proc.interruptChildOnly True)
            "sh" ["-c", "kill -INT $$"]
    r1 `shouldBe` Left UserInterrupt
    failsWith (-2)
        (S.fold Fold.drain (Proc.toChunks "sh" ["-c", "kill -INT $$"]))
        >>= (`shouldBe` True)

-- | Whether the descriptor is open in a process run by @standalone True@.
-- @test -e@ is a shell builtin, the shell does not open any file before it.
standaloneFdOpen :: (Bool, Bool, Bool) -> Int -> IO Bool
standaloneFdOpen closeStreams fd = do
    r <- Proc.standalone True closeStreams id
            "sh" ["-c", "test -e /dev/fd/" ++ show fd]
    case r of
        Left code -> return (code == ExitSuccess)
        Right _ -> error "standalone returned a handle"

standaloneCloseStreams :: IO ()
standaloneCloseStreams = do
    closed <- mapM (uncurry standaloneFdOpen)
        [ ((True, False, False), 0)
        , ((False, True, False), 1)
        , ((False, False, True), 2)
        ]
    closed `shouldBe` [False, False, False]
    inherited <- mapM (standaloneFdOpen (False, False, False)) [0, 1, 2]
    inherited `shouldBe` [True, True, True]

-------------------------------------------------------------------------------
-- Config modifier coverage
-------------------------------------------------------------------------------

setCwdTest :: IO ()
setCwdTest = do
    toStringWith (Proc.setCwd (Just "/")) "pwd" [] >>= (`shouldBe` "/\n")
    cwd <- getCurrentDirectory >>= canonicalizePath
    toStringWith (Proc.setCwd Nothing) "pwd" ["-P"]
        >>= (`shouldBe` cwd ++ "\n")

setEnvTest :: IO ()
setEnvTest = do
    envPath <- which "env"
    toStringWith
        (Proc.setEnv (Just [("STREAMLY_PROCESS_TEST", "1")])) envPath []
        >>= (`shouldBe` "STREAMLY_PROCESS_TEST=1\n")
    setEnv "STREAMLY_PROCESS_TEST" "2"
    toStringWith (Proc.setEnv Nothing)
        "sh" ["-c", "echo $STREAMLY_PROCESS_TEST"]
        >>= (`shouldBe` "2\n")

closeFilesTest :: IO ()
#if defined(mingw32_HOST_OS)
closeFilesTest = return ()
#else
closeFilesTest = do
    (r, w) <- createPipe
    let isOpenInChild cl =
            not <$> failsWith 1
                (toStringWith (Proc.closeFiles cl)
                    "sh" ["-c", "test -e /dev/fd/" ++ show (fromEnum r)])
    open1 <- isOpenInChild False
    open2 <- isOpenInChild True
    closeFd r
    closeFd w
    (open1, open2) `shouldBe` (True, False)
#endif

newProcessGroupTest :: IO ()
newProcessGroupTest = do
    isGroupLeader (Proc.newProcessGroup True) >>= (`shouldBe` True)
    isGroupLeader (Proc.newProcessGroup False) >>= (`shouldBe` False)

setSessionTest :: IO ()
setSessionTest = do
    isGroupLeader id >>= (`shouldBe` False)
    isGroupLeader (Proc.setSession Proc.NewSession) >>= (`shouldBe` True)
    isGroupLeader (Proc.setSession Proc.InheritSession) >>= (`shouldBe` False)
    -- NewConsole has no effect on POSIX
    isGroupLeader (Proc.setSession Proc.NewConsole) >>= (`shouldBe` False)

-- | The signals ignored by the parent, as seen by the child process. Linux
-- only, returns Nothing if /proc is not available.
parentIgnoredSignals :: (Proc.Config -> Proc.Config) -> IO (Maybe Integer)
parentIgnoredSignals modCfg = do
    hasProc <- doesFileExist "/proc/self/status"
    if not hasProc
    then return Nothing
    else do
        out <- toStringWith modCfg
                "sh" ["-c", "grep SigIgn /proc/$PPID/status"]
        case readHex (dropWhile (`elem` " \t") (drop 7 out)) of
            [(mask, _)] | "SigIgn:" `isPrefixOf` out -> return (Just mask)
            _ -> error $ "parentIgnoredSignals: bad output " ++ show out

-- | SIGINT is 2 and SIGQUIT is 3, their bits in the mask are 0x2 and 0x4.
interruptChildOnlyTest :: IO ()
interruptChildOnlyTest = do
    m1 <- parentIgnoredSignals (Proc.interruptChildOnly True)
    m2 <- parentIgnoredSignals (Proc.interruptChildOnly False)
    fmap (.&. 6) m1 `shouldSatisfy` maybe True (== 6)
    fmap (.&. 6) m2 `shouldSatisfy` maybe True (== 0)

-- | The process package calls initgroups when the user id is set, which
-- needs root privileges even if the user id does not change.
setUserIdTest :: IO ()
setUserIdTest = do
    uid <- read <$> Proc.toString "id" ["-u"]
    toStringWith (Proc.setUserId Nothing) "id" ["-u"]
        >>= (`shouldBe` show uid ++ "\n")
    r <- try (toStringWith (Proc.setUserId (Just uid)) "id" ["-u"])
    if uid == 0
    then r `shouldBe` Right "0\n"
    else case r of
        Left (_ :: IOException) -> return ()
        Right v -> expectationFailure $ "expected an exception, got " ++ v

setGroupIdTest :: IO ()
setGroupIdTest = do
    gid <- read <$> Proc.toString "id" ["-g"]
    toStringWith (Proc.setGroupId (Just gid)) "id" ["-g"]
        >>= (`shouldBe` show gid ++ "\n")
    toStringWith (Proc.setGroupId Nothing) "id" ["-g"]
        >>= (`shouldBe` show gid ++ "\n")

-- | No effect on POSIX.
waitForDescendantsTest :: IO ()
waitForDescendantsTest = do
    toStringWith (Proc.waitForDescendants True) "echo" ["x"]
        >>= (`shouldBe` "x\n")
    toStringWith (Proc.waitForDescendants False) "echo" ["x"]
        >>= (`shouldBe` "x\n")

-------------------------------------------------------------------------------
-- Streamly.Internal.System.Process API coverage
-------------------------------------------------------------------------------

pipeCharsCat :: IO ()
pipeCharsCat =
    S.fold Fold.toList (IProc.pipeChars "cat" [] (S.fromList utf8Text))
        >>= (`shouldBe` utf8Text)

-- | The internal modifiers already applied by an API leave it unchanged.
internalModifiers :: IO ()
internalModifiers = do
    toStringWith IProc.inheritStdin "echo" ["x"] >>= (`shouldBe` "x\n")
    toStringWith IProc.pipeStdout "echo" ["x"] >>= (`shouldBe` "x\n")
    eitherToLists
        (IProc.toChunksEitherWith (IProc.pipeStdErr . IProc.pipeStdout)
            "sh" ["-c", "printf o; printf e >&2"])
        >>= (`shouldBe` (ascii "e", ascii "o"))
    S.fold Fold.toList
        (concatArr
            (IProc.pipeChunksWith IProc.pipeStdin "cat" []
                (S.fromPure (Array.fromList (ascii "a")))))
        >>= (`shouldBe` ascii "a")

-------------------------------------------------------------------------------
-- Streamly.System.Command API coverage
-------------------------------------------------------------------------------

commandGeneration :: IO ()
commandGeneration = do
    S.fold Fold.toList (Command.toBytes "echo 'hello world'")
        >>= (`shouldBe` ascii "hello world\n")
    S.fold Fold.toList (concatArr (Command.toChunks "echo hello"))
        >>= (`shouldBe` ascii "hello\n")
    S.fold Fold.toList
        (concatArr (Command.toChunksWith (Proc.setCwd (Just "/")) "pwd"))
        >>= (`shouldBe` ascii "/\n")
    S.fold Fold.toList (Command.toChars "printf 'h\\303\\251\\n'")
        >>= (`shouldBe` utf8Text)
    S.fold Fold.toList (Command.toLines Fold.toList "printf 'a\\nb'")
        >>= (`shouldBe` ["a", "b"])

commandEffects :: IO ()
commandEffects = do
    Command.toString "echo \"hello  world\"" >>= (`shouldBe` "hello  world\n")
    captureStdout (Command.toStdout "echo hello") >>= (`shouldBe` "hello\n")
    Command.toNull "echo hello" >>= (`shouldBe` ())
    failsWith 1 (Command.toNull "false") >>= (`shouldBe` True)

commandTransformation :: IO ()
commandTransformation = do
    let input = ascii "hello"
    S.fold Fold.toList (Command.pipeBytes "tr a-z A-Z" (S.fromList input))
        >>= (`shouldBe` ascii "HELLO")
    S.fold Fold.toList
        (concatArr
            (Command.pipeChunks "tr a-z A-Z"
                (S.fromPure (Array.fromList input))))
        >>= (`shouldBe` ascii "HELLO")
    S.fold Fold.toList
        (concatArr
            (Command.pipeChunksWith (Proc.setCwd (Just "/"))
                "sh -c 'cat; pwd'"
                (S.fromPure (Array.fromList input))))
        >>= (`shouldBe` ascii "hello/\n")
    S.fold Fold.toList (Command.pipeChars "tr a-z A-Z" (S.fromList "h\233"))
        >>= (`shouldBe` "H\233")

commandStandalone :: IO ()
commandStandalone = do
    Command.foreground id "sh -c 'exit 3'" >>= (`shouldBe` ExitFailure 3)
    r <- Command.standalone True (True, True, True) id "sh -c 'exit 4'"
    either Just (const Nothing) r `shouldBe` Just (ExitFailure 4)
    ph <- Command.daemon id "sleep 10"
    threadDelay settleTime
    st <- getProcessExitCode ph
    terminateProcess ph
    _ <- waitForProcess ph
    st `shouldBe` Nothing

commandWithModifiers :: IO ()
commandWithModifiers = do
    Cmd.runWith Proc.toString "echo a  'b  c'" >>= (`shouldBe` "a b  c\n")
    S.fold Fold.toList (Cmd.streamWith Proc.toBytes "echo a")
        >>= (`shouldBe` ascii "a\n")
    S.fold Fold.toList
        (Cmd.pipeWith Proc.pipeBytes "tr a A" (S.fromList (ascii "ab")))
        >>= (`shouldBe` ascii "Ab")

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

            describe "process does not read all input" $ do
                it "pipeBytes head -c 1" pipeBytesPartialInput
                it "pipeBytes head -c 1, infinite input" pipeBytesInfiniteInput
                it "pipeChunks true" pipeChunksNoInputRead
                it "pipeChunksEither closing stdin" pipeChunksEitherStdinClosed
                it "pipeBytesEither head -c 1" pipeBytesEitherPartialInput
                it "pipeChunks failing without reading"
                    pipeChunksNoInputReadFailure

            describe "inheritStdout" $ do
                it "pipeChunksEitherWith inheritStdout"
                    pipeChunksEitherInheritStdout
                it "toChunksEitherWith inheritStdout"
                    toChunksEitherInheritStdout

            describe "config modifiers" $ do
                it "setSession InheritSession after NewSession"
                    setSessionLastWins
                it "setCwd" setCwdTest
                it "setEnv" setEnvTest
                it "closeFiles" closeFilesTest
                it "newProcessGroup" newProcessGroupTest
                it "setSession" setSessionTest
                it "interruptChildOnly" interruptChildOnlyTest
                it "setUserId" setUserIdTest
                it "setGroupId" setGroupIdTest
                it "waitForDescendants" waitForDescendantsTest

            describe "exceptions" $ do
                it "ProcessFailure displayException" processFailureDisplay
                it "ProcessFailure on a signal" processFailureSignal
                it "executable does not exist" execFailure

            describe "generation" $ do
                it "toChunks" toChunksEcho
                it "toBytes" toBytesEcho
                it "toBytesWith" toBytesWithCwd
                it "toChars" toCharsUtf8
                it "toChars with invalid UTF-8" toCharsInvalidUtf8
                it "toLines" toLinesSplit
                it "toChunksEitherWith" toChunksEitherWithCwd

            describe "effects" $ do
                it "toString" toStringEcho
                it "toString on failure" toStringFailure
                it "toStdout" toStdoutEcho
                it "toNull" toNullEcho
                it "toNull on failure" toNullFailure

            describe "transformation" $ do
                it "pipeChunksWith" pipeChunksWithEnv
                it "pipeChunksEitherWith" pipeChunksEitherWithCwd

            describe "internal" $ do
                it "pipeChars" pipeCharsCat
                it "stdio modifiers" internalModifiers

            describe "standalone processes" $ do
                it "foreground" foregroundExitCode
                it "foreground, child killed by SIGINT" foregroundSigInt
                it "standalone closes stdio" standaloneCloseStreams
                it "daemon keeps the process running" daemonKeepsRunning
                it "standalone False keeps the process running"
                    standaloneNoWaitKeepsRunning
                it "standalone True returns the exit code"
                    standaloneWaitExitCode

        describe "Streamly.System.Command" $ do
            it "generation" commandGeneration
            it "effects" commandEffects
            it "transformation" commandTransformation
            it "standalone processes" commandStandalone
            it "runWith, streamWith, pipeWith" commandWithModifiers

            describe "invalid command strings" $ do
                let forEach name f =
                        mapM_ (\c -> it (name ++ " " ++ show c) (f c))
                            invalidCommands
                forEach "toString" invalidCommandRunWith
                forEach "toBytes" invalidCommandStreamWith
                forEach "pipeBytes" invalidCommandPipeWith

            describe "shellWord" $ do
                it "Single quote test" $
                   shellWordTest "'hello\\\\\"world'" ["hello\\\\\"world"]
                it "Double quote test" $
                   shellWordTest
                       "\"hello\\\"\\\\w\\'orld\""
                       ["hello\"\\w\\'orld"]
#if MIN_VERSION_streamly_core(0,3,0)
                it "Double quote test, escape at end" $
                    shellWordTest "'hello\\'" ["hello\\"]
#endif
                -- POSIX: in double quotes a backslash escapes only the
                -- characters dollar, backquote, double quote, backslash.
                it "Double quote, escaped dollar" $
                    shellWordTest "\"a\\$b\"" ["a$b"]
                it "Double quote, escaped backquote" $
                    shellWordTest "\"a\\`b\"" ["a`b"]
                it "Double quote, backslash before other char is kept" $
                    shellWordTest "\"a\\b\"" ["a\\b"]
                it "Unquoted escaped space" $
                    shellWordTest "a\\ b" ["a b"]
                it "Multiple spaces" $
                    shellWordTest " a  b " ["a", "b"]
                it "Adjacent quoted parts form one word" $
                    shellWordTest "'a'\"b\"c" ["abc"]
                it "Empty quotes are an empty word" $
                    shellWordTest "\"\" ''" ["", ""]
