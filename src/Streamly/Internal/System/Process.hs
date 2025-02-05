-- |
-- Module      : Streamly.Internal.System.Process
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO:
--
-- Remove dependency on the "process" package. We do not need a lot of it or
-- need to reimplement significantly.
--
-- Interactive processes:
--
-- To understand process groups and sessions, see the "General Terminal
-- Interface" section in the [POSIX
-- standard](https://pubs.opengroup.org/onlinepubs/9699919799/).
--
-- For running processes interactively we need to make a new process group for
-- the new process and make it the foreground process group. When the process
-- is done we can make the parent process group as the foreground process group
-- again. We need to ensure that this works properly under exceptions. We can
-- provide an "interact" function to do so.
--
-- - Need a way to specify additional parameters for process creation.
-- Possibly use something like @processBytesWith spec@ etc.
--
-- - Need a way to access the pid and manage the processes and process groups.
-- We can treat the processes in the same way as we treat threads. We can
-- compose processes in parallel, and cleanup can happen in the same way as
-- tids are cleaned up. But do we need this when we have threads anyway?
--
-- - Use unfolds for generation?
--
-- - Folds for composing process sinks? Input may be taken as input of the
-- fold and the output of the process can be consumed by another fold.
--
-- - Replace FilePath with a typed path.
--

module Streamly.Internal.System.Process
    (
    -- * Process Configuration
      Config

    -- ** Common Config Options
    -- | These options apply to both POSIX and Windows.
    , setCwd
    , setEnv
    {-
    , setStdin
    , setStdout
    , setStderr
    -}
    , closeFiles
    , newProcessGroup
    , Session (..)
    , setSession

    -- * Posix Only Options
    -- | These options have no effect on Windows.
    , interruptChildOnly
    , setUserId
    , setGroupId

    -- * Windows Only Options
    -- | These options have no effect on Posix.
    , waitForDescendants

    -- * Internal
    , inheritStdin
    , inheritStdout
    , pipeStdErr
    , pipeStdin
    , pipeStdout

    -- * Exceptions
    , ProcessFailure (..)

    -- * Generation
    -- | stdout of the process is redirected to output stream.
    , toBytes
    , toChunks
    , toChunksWith
    , toChars
    , toLines
    , toString
    , toStdout
    , toNull

    -- * Transformation
    -- | The input stream is redirected to the stdin of the process, stdout of
    -- the process is redirected to the output stream.
    , pipeBytes
    , pipeChunks
    , pipeChunksWith
    , pipeChars

    -- * Stderr
    -- | Like other "Generation" routines but along with stdout, stderr is also
    -- included in the output stream. stdout is converted to 'Right' values in
    -- the output stream and stderr is converted to 'Left' values.
    , toBytesEither
    , toChunksEither
    , toChunksEitherWith
    , pipeBytesEither
    , pipeChunksEither
    , pipeChunksEitherWith

    -- * Standalone Processes
    , foreground
    , daemon
    , standalone

    -- * Deprecated
    , parentIgnoresInterrupt
    , waitForChildTree
    , interactive
    , processBytes
    , processChunks
    )
where

import Control.Concurrent (forkIO)
import Control.Exception (Exception(..), catch, throwIO)
import Control.Monad (void, unless)
import Control.Monad.Catch (MonadCatch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.Word (Word8, Word32)
import Foreign.C.Error (Errno(..), ePIPE)
import GHC.IO.Exception (IOException(..), IOErrorType(..))
import Streamly.Data.Array (Array)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Stream.Prelude (MonadAsync, Stream)
import System.Exit (ExitCode(..))
import System.IO (hClose, Handle, hSetBuffering, BufferMode(..))
#if !defined(mingw32_HOST_OS)
import System.Posix.Types (CUid (..), CGid (..))
#endif

#ifdef USE_NATIVE
import Control.Exception (SomeException)
import System.Posix.Process (ProcessStatus(..))
import Streamly.Internal.System.Process.Posix
#else
import System.Process
    ( ProcessHandle
    , CreateProcess(..)
    , StdStream (..)
    , createProcess
    , waitForProcess
    , CmdSpec(..)
    , terminateProcess
    , withCreateProcess
    )
#endif

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold

-- Internal imports
import Streamly.Internal.System.IO (defaultChunkSize)

import qualified Streamly.Internal.Console.Stdio as Stdio (putChunks)
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold (either)
import qualified Streamly.Internal.FileSystem.Handle
    as Handle (readChunks, putChunks)
import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.Internal.Unicode.Stream as Unicode (lines)

#include "DocTestProcess.hs"

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
-- Config
-------------------------------------------------------------------------------

-- | Process configuration used for creating a new process.
--
-- By default the process config is setup to inherit the following attributes
-- from the parent process:
--
-- * Current working directory
-- * Environment variables
-- * Open file descriptors
-- * Process group
-- * Terminal session
--
-- On POSIX:
--
-- * Process uid and gid
-- * Signal handlers
--
-- On Windows by default the parent process waits for the entire child process
-- tree to finish.
--
#ifdef USE_NATIVE

type ProcessHandle = Process

-- XXX After the fork specify what code to run in parent and in child before
-- exec. Also, use config to control whether to search the binary in the PATH
-- or not.
newtype Config = Config Bool

mkConfig :: FilePath -> [String] -> Config
mkConfig _ _ = Config False

pipeStdErr :: Config -> Config
pipeStdErr (Config _) = Config True

pipeStdin :: Config -> Config
pipeStdin (Config _) = Config True

pipeStdout :: Config -> Config
pipeStdout (Config _) = Config True

inheritStdin :: Config -> Config
inheritStdin (Config _) = Config True

inheritStdout :: Config -> Config
inheritStdout (Config _) = Config True

#else

newtype Config = Config CreateProcess

-- | Create a default process configuration from an executable file path and
-- an argument list.
--
mkConfig :: FilePath -> [String] -> Config
mkConfig path args = Config $ CreateProcess
    { cmdspec = RawCommand path args
    , cwd = Nothing -- inherit
    , env = Nothing -- inherit

    -- File descriptors
    , std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = Inherit
    , close_fds = False

    -- Session/group/setuid/setgid
    , create_group = False
    , child_user = Nothing  -- Posix only
    , child_group = Nothing  -- Posix only

    -- Signals (Posix only) behavior
    -- Ignore SIGINT (Ctrl-C) and SIGQUIT (Ctrl-\) in the parent process until
    -- the child exits i.e. let the child handle it. See
    -- https://www.cons.org/cracauer/sigint.html .
    , delegate_ctlc = False

    -- Terminal behavior
    , new_session = False  -- Posix only
    , detach_console = False -- Windows only
    , create_new_console = False -- Windows Only

    -- Added by commit 6b8ffe2ec3d115df9ccc047599545ca55c393005
    , use_process_jobs = True -- Windows only
    }

-- XXX use osPath

-- | Set the current working directory of the new process. When 'Nothing', the
-- working directory is inherited from the parent process.
--
-- Default is 'Nothing' - inherited from the parent process.
setCwd :: Maybe FilePath -> Config -> Config
setCwd path (Config cfg) = Config $ cfg { cwd = path }

-- | Set the environment variables for the new process. When 'Nothing', the
-- environment is inherited from the parent process.
--
-- Default is 'Nothing' - inherited from the parent process.
setEnv :: Maybe [(String, String)] -> Config -> Config
setEnv e (Config cfg) = Config $ cfg { env = e }

{-
-- XXX We should allow setting only those stdio streams which are not used for
-- piping. We can either close those or inherit from parent.
--
-- * In a source we have to close stdin and use stdout
-- * In a pipe we have to use both
-- * In a sink we have to close stdout and use stdin.
--
-- Only stderr may be left for setting - either pipe it to merge it with stdout
-- or inherit or close. Closing may lead to bad behavior in most cases. So it
-- is either inherit or merge with stdout. Merge with stdout can be achieved by
-- using the either combinators. So there is nothing really left to set here
-- for any std stream.

-- UseProc, UsePipe?
-- | What to do with the stdin, stdout, stderr streams of the process.
data Stdio =
      ToHaskell -- ^ Pipe to Haskell Streamly
    | ToProcess -- ^ Connect to the parent process

toStdStream :: Stdio -> StdStream
toStdStream x =
    case x of
        ToProcess -> Inherit
        ToHaskell -> CreatePipe

-- | What to do with the @stdin@ stream of the process.
setStdin :: Stdio -> Config -> Config
setStdin x (Config cfg) = Config $ cfg { std_in = toStdStream x }

-- | What to do with the @stdout@ stream of the process.
setStdout :: Stdio -> Config -> Config
setStdout x (Config cfg) = Config $ cfg { std_out = toStdStream x }

-- | What to do with the @stderr@ stream of the process.
setStderr :: Stdio -> Config -> Config
setStderr x (Config cfg) = Config $ cfg { std_err = toStdStream x }
-}

-- | Close all open file descriptors inherited from the parent process. Note,
-- this does not apply to stdio descriptors - the behavior of those is determined
-- by other configuration settings.
--
-- Default is 'False'.
--
-- Note: if the number of open descriptors is large, it may take a while
-- closing them.
closeFiles :: Bool -> Config -> Config
closeFiles x (Config cfg) = Config $ cfg { close_fds = x }

-- XXX Do these details apply to Windows as well?

-- | If 'True' the new process starts a new process group, becomes a process
-- group leader, its pid becoming the process group id.
--
-- See the POSIX
-- <https://man7.org/linux/man-pages/man2/setpgid.2.html setpgid>
-- man page.
--
-- Default is 'False', the new process belongs to the parent's process group.
newProcessGroup :: Bool -> Config -> Config
newProcessGroup x (Config cfg) = Config $ cfg { create_group = x }

-- | 'InheritSession' makes the new process inherit the terminal session from the
-- parent process. This is the default.
--
-- 'NewSession' makes the new process start with a new session without a
-- controlling terminal. On POSIX,
-- <https://man7.org/linux/man-pages/man2/setsid.2.html setsid>
-- is used to create a new process
-- group and session, the pid of the new process is the session id and process
-- group id as well. On Windows @DETACHED_PROCESS@ flag is used to detach the
-- process from inherited console session.
--
-- 'NewConsole' creates a new terminal and attaches the process to the new
-- terminal on Windows, using the CREATE_NEW_CONSOLE flag. On POSIX this does
-- nothing.
--
-- For Windows see
--
-- * https://learn.microsoft.com/en-us/windows/win32/procthread/process-creation-flags
-- * https://learn.microsoft.com/en-us/windows/console/creation-of-a-console .
--
-- For POSIX see, <https://man7.org/linux/man-pages/man2/setsid.2.html setsid>
-- man page.
data Session =
      InheritSession -- ^ Inherit the parent session
    | NewSession -- ^ Detach process from the current session
    | NewConsole -- ^ Windows only, CREATE_NEW_CONSOLE flag

-- | Define the terminal session behavior for the new process.
--
-- Default is 'InheritSession'.
setSession :: Session -> Config -> Config
setSession x (Config cfg) =
    Config $
        case x of
            InheritSession -> cfg
            NewSession -> cfg { new_session = True}
            NewConsole -> cfg {create_new_console = True}

-- | Use the POSIX
-- <https://man7.org/linux/man-pages/man2/setuid.2.html setuid>
-- call to set the user id of the new process before
-- executing the command. The parent process must have sufficient privileges to
-- set the user id.
--
-- POSIX only. See the POSIX
-- <https://man7.org/linux/man-pages/man2/setuid.2.html setuid>
-- man page.
--
-- Default is 'Nothing' - inherit from the parent.
setUserId :: Maybe Word32 -> Config -> Config
#if defined(mingw32_HOST_OS)
setUserId _ (Config cfg) =
    Config cfg
#else
setUserId x (Config cfg) =
    Config $ cfg { child_user = CUid <$> x }
#endif

-- | Use the POSIX
-- <https://man7.org/linux/man-pages/man2/setgid.2.html setgid>
-- call to set the group id of the new process before
-- executing the command. The parent process must have sufficient privileges to
-- set the group id.
--
-- POSIX only. See the POSIX
-- <https://man7.org/linux/man-pages/man2/setgid.2.html setgid>
-- man page.
--
-- Default is 'Nothing' - inherit from the parent.
setGroupId :: Maybe Word32 -> Config -> Config
#if defined(mingw32_HOST_OS)
setGroupId _ (Config cfg) =
    Config cfg
#else
setGroupId x (Config cfg) =
    Config $ cfg { child_group = CGid <$> x }
#endif

-- See https://www.cons.org/cracauer/sigint.html for more details on signal
-- handling by interactive processes.

-- | When this is 'True', the parent process ignores user interrupt signals
-- @SIGINT@ and @SIGQUIT@ delivered to it until the child process exits. If
-- multiple child processes are started then the default handling in the parent
-- is restored only after the last one exits.
--
-- When a user presses CTRL-C or CTRL-\ on the terminal, a SIGINT or SIGQUIT is
-- sent to all the foreground processes in the terminal session, this includes
-- both the child and the parent. By default, on receiving these signals, the
-- parent process would cleanup and exit, to avoid that and let the child
-- handle these signals we can choose to ignore these signals in the parent
-- until the child exits.
--
-- POSIX only. Default is 'False'.
interruptChildOnly :: Bool -> Config -> Config
interruptChildOnly x (Config cfg) = Config $ cfg { delegate_ctlc = x }

{-# DEPRECATED parentIgnoresInterrupt "Use interruptChildOnly instead." #-}
parentIgnoresInterrupt :: Bool -> Config -> Config
parentIgnoresInterrupt = interruptChildOnly

-- | On Windows, the parent waits for the entire descendant tree of process
-- i.e. including processes that are spawned by the child process.
--
-- Default is 'True'.
waitForDescendants :: Bool -> Config -> Config
waitForDescendants x (Config cfg) = Config $ cfg { use_process_jobs = x }

{-# DEPRECATED waitForChildTree "Use waitForDescendants instead." #-}
waitForChildTree :: Bool -> Config -> Config
waitForChildTree = waitForDescendants

pipeStdErr :: Config -> Config
pipeStdErr (Config cfg) = Config $ cfg { std_err = CreatePipe }

pipeStdin :: Config -> Config
pipeStdin (Config cfg) = Config $ cfg { std_in = CreatePipe }

pipeStdout :: Config -> Config
pipeStdout (Config cfg) = Config $ cfg { std_out = CreatePipe }

inheritStdin :: Config -> Config
inheritStdin (Config cfg) = Config $ cfg { std_in = Inherit }

inheritStdout :: Config -> Config
inheritStdout (Config cfg) = Config $ cfg { std_out = Inherit }
#endif

-------------------------------------------------------------------------------
-- Exceptions
-------------------------------------------------------------------------------
--
-- TODO Add the path of the executable and the PID of the process to the
-- exception info to aid debugging.

-- | An exception that is raised when a process fails.
--
-- @since 0.1.0
newtype ProcessFailure = ProcessFailure Int -- ^ The exit code of the process.
    deriving Show

-- Exception instance of ProcessFailure
instance Exception ProcessFailure where

    displayException (ProcessFailure exitCode) =
        "Process failed with exit code: " ++ show exitCode

parallel :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
parallel s1 s2 = Stream.parList (Stream.eager True) [s1, s2]

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

-- | On normal cleanup we do not need to close the pipe handles as they are
-- already guaranteed to be closed (we can assert that) by the time we reach
-- here. We should not kill the process, rather wait for it to terminate
-- normally.
cleanupNormal ::
    (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
cleanupNormal (_, _, _, procHandle) = do
#ifdef USE_NATIVE
    -- liftIO $ putStrLn "cleanupNormal waiting"
    status <- wait procHandle
    -- liftIO $ putStrLn "cleanupNormal done"
    case status of
        Exited ExitSuccess -> return ()
        Exited (ExitFailure code) -> throwM $ ProcessFailure code
        Terminated signal _ ->
            throwM $ ProcessFailure (negate $ fromIntegral signal)
        Stopped signal ->
            throwM $ ProcessFailure (negate $ fromIntegral signal)
#else
    exitCode <- waitForProcess procHandle
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> throwM $ ProcessFailure code
#endif

-- | On an exception or if the process is getting garbage collected we need to
-- close the pipe handles, and send a SIGTERM to the process to clean it up.
-- Since we are using SIGTERM to kill the process, it may block forever. We can
-- possibly use a timer and send a SIGKILL after the timeout if the process is
-- still hanging around.
cleanupException ::
    (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
cleanupException (stdinMaybe, stdoutMaybe, stderrMaybe, ph) = do
    -- Send a SIGTERM to the process
#ifdef USE_NATIVE
    terminate ph
#else
    terminateProcess ph
#endif

    -- Ideally we should be closing the handle without flushing the buffers so
    -- that we cannot get a SIGPIPE. But there seems to be no way to do that as
    -- of now so we just ignore the SIGPIPE.
    whenJust (\stdinH -> hClose stdinH `catch` eatSIGPIPE) stdinMaybe
    whenJust hClose stdoutMaybe
    whenJust hClose stderrMaybe

    -- Non-blocking wait for the process to go away
#ifdef USE_NATIVE
    void $ forkIO (void $ wait ph)
#else
    void $ forkIO (void $ waitForProcess ph)
#endif

    where

    whenJust action mb = maybe (pure ()) action mb

    isSIGPIPE e =
        case e of
            IOError
                { ioe_type = ResourceVanished
                , ioe_errno = Just ioe
                } -> Errno ioe == ePIPE
            _ -> False

    eatSIGPIPE e = unless (isSIGPIPE e) $ throwIO e

-- | Creates a system process from an executable path and arguments. For the
-- default attributes used to create the process see 'mkConfig'.
--
createProc' ::
       (Config -> Config) -- ^ Process attribute modifier
    -> FilePath                         -- ^ Executable path
    -> [String]                         -- ^ Arguments
    -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
    -- ^ (Input Handle, Output Handle, Error Handle, Process Handle)
createProc' modCfg path args = do
#ifdef USE_NATIVE
    ((inp, out, err, _excParent, _excChild), parent, child, failure) <-
        mkStdioPipes cfg
    -- XXX Pass the exChild handle to the child process action
    proc <- newProcess child path args Nothing
              `catch` (\(e :: SomeException) -> failure >> throwIO e)
    -- XXX Read the exception channel and reap the process if it failed before
    -- exec.
    parent
    hSetBuffering inp NoBuffering
    hSetBuffering out NoBuffering
    hSetBuffering err NoBuffering
    return (Just inp, Just out, err, proc)
#else
    r@(inp, out, err, _) <- createProcess cfg
    mapM_ (`hSetBuffering` NoBuffering) inp
    mapM_ (`hSetBuffering` NoBuffering) out
    mapM_ (`hSetBuffering` NoBuffering) err
    return r
#endif

    where

    Config cfg = modCfg $ mkConfig path args

{-# INLINE putChunksClose #-}
putChunksClose :: MonadIO m =>
    Handle -> Stream m (Array Word8) -> Stream m a
putChunksClose h input =
    Stream.before
        (Handle.putChunks h input >> liftIO (hClose h))
        Stream.nil

{-# INLINE toChunksClose #-}
toChunksClose :: MonadAsync m => Handle -> Stream m (Array Word8)
toChunksClose h = Stream.afterIO (hClose h) (Handle.readChunks h)

{-# INLINE pipeChunksWithAction #-}
pipeChunksWithAction ::
    (MonadCatch m, MonadAsync m)
    => ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> Stream m a)
    -> (Config -> Config)
    -> FilePath             -- ^ Path to Executable
    -> [String]             -- ^ Arguments
    -> Stream m a     -- ^ Output stream
pipeChunksWithAction run modCfg path args =
    Stream.bracketIO3
          alloc cleanupNormal cleanupException cleanupException run

    where

    alloc = createProc' modCfg path args

-- Note: It is allowed to inheritStdout or inheritStderr but not both as that
-- would not generate a stream for further processing.
-- inheritStdin has no affect, we always pipe the input stream to the process's
-- stdin

-- | Like 'pipeChunksEither' but use the specified configuration to run the
-- process.
{-# INLINE pipeChunksEitherWith #-}
pipeChunksEitherWith ::
    (MonadCatch m, MonadAsync m)
    => (Config -> Config)   -- ^ Config modifier
    -> FilePath             -- ^ Executable name or path
    -> [String]             -- ^ Arguments
    -> Stream m (Array Word8)    -- ^ Input stream
    -> Stream m (Either (Array Word8) (Array Word8))     -- ^ Output stream
pipeChunksEitherWith modifier path args input =
    pipeChunksWithAction run (pipeStdin . modifier . pipeStdErr) path args

    where

    run (_, Nothing, Nothing, _) =
        error "pipeChunksEitherWith: only one of stdout or stderr can be inherited"
    run (Just stdinH, Just stdoutH, Just stderrH, _) =
        putChunksClose stdinH input
            `parallel` fmap Left (toChunksClose stderrH)
            `parallel` fmap Right (toChunksClose stdoutH)
    run _ = error "pipeChunksEitherWith: Not reachable"

-- | Like 'pipeChunks' but also includes stderr as 'Left' stream in the
-- 'Either' output.
--
-- Definition:
--
-- >>> pipeChunksEither = pipeChunksEitherWith id
--
{-# INLINE pipeChunksEither #-}
pipeChunksEither ::
    (MonadCatch m, MonadAsync m)
    => FilePath             -- ^ Executable name or path
    -> [String]             -- ^ Arguments
    -> Stream m (Array Word8)    -- ^ Input stream
    -> Stream m (Either (Array Word8) (Array Word8))     -- ^ Output stream
pipeChunksEither = pipeChunksEitherWith id

-- | @pipeBytesEither path args input@ runs the executable at @path@ using
-- @args@ as arguments and @input@ stream as its standard input.  The error
-- stream of the executable is presented as 'Left' values in the resulting
-- stream and output stream as 'Right' values. The input to the pipe is
-- buffered with a buffer size of 'defaultChunkSize'.
--
-- For control over the buffer use your own chunking and chunk based APIs.
--
-- Raises 'ProcessFailure' exception in case of failure.
--
-- For example, the following is equivalent to the shell command @echo "hello
-- world" | tr [:lower:] [:upper:]@:
--
-- >>> :{
--    pipeBytesEither "echo" ["hello world"] Stream.nil
--  & Stream.catRights
--  & pipeBytesEither "tr" ["[:lower:]", "[:upper:]"]
--  & Stream.catRights
--  & Stream.fold Stdio.write
--  :}
--HELLO WORLD
--
-- @since 0.1.0
{-# INLINE pipeBytesEither #-}
pipeBytesEither ::
    (MonadCatch m, MonadAsync m)
    => FilePath         -- ^ Executable name or path
    -> [String]         -- ^ Arguments
    -> Stream m Word8        -- ^ Input Stream
    -> Stream m (Either Word8 Word8) -- ^ Output Stream
pipeBytesEither path args input =
    let input1 = CHUNKS_OF defaultChunkSize input
        output = pipeChunksEither path args input1
        leftRdr = fmap Left Array.reader
        rightRdr = fmap Right Array.reader
     in UNFOLD_EACH (Unfold.either leftRdr rightRdr) output

-- Note: inheritStdin, inheritStdout have no affect, we always pipe
-- the input stream to the process's stdin and pipe the stdout to the
-- resulting stream

-- | Like 'pipeChunks' but use the specified configuration to run the process.
{-# INLINE pipeChunksWith #-}
pipeChunksWith ::
    (MonadCatch m, MonadAsync m)
    => (Config -> Config)   -- ^ Config modifier
    -> FilePath             -- ^ Executable name or path
    -> [String]             -- ^ Arguments
    -> Stream m (Array Word8)    -- ^ Input stream
    -> Stream m (Array Word8)    -- ^ Output stream
pipeChunksWith modifier path args input =
    pipeChunksWithAction run (pipeStdout . pipeStdin . modifier) path args

    where

    run (Just stdinH, Just stdoutH, _, _) =
        putChunksClose stdinH input
            `parallel` toChunksClose stdoutH
    run _ = error "pipeChunksWith: unreachable"

-- | @pipeChunks file args input@ runs the executable @file@ specified by
-- its name or path using @args@ as arguments and @input@ stream as its
-- standard input.  Returns the standard output of the process as a stream
-- of chunks of bytes (Array Word8).
--
-- If only the name of an executable file is specified instead of its path then
-- the file name is searched in the directories specified by the PATH
-- environment variable.
--
-- If the input stream throws an exception or if the output stream is garbage
-- collected before it could finish then the process is terminated with SIGTERM.
--
-- If the process terminates with a non-zero exit code then a 'ProcessFailure'
-- exception is raised.
--
-- The following code is equivalent to the shell command @echo "hello world" |
-- tr [a-z] [A-Z]@:
--
-- >>> :{
--    Process.toChunks "echo" ["hello world"]
--  & Process.pipeChunks "tr" ["[a-z]", "[A-Z]"]
--  & Stream.fold Stdio.writeChunks
--  :}
--HELLO WORLD
--
-- Definition:
--
-- >>> pipeChunks = pipeChunksWith id
--
-- /pre-release/
{-# INLINE pipeChunks #-}
pipeChunks ::
    (MonadCatch m, MonadAsync m)
    => FilePath             -- ^ Executable name or path
    -> [String]             -- ^ Arguments
    -> Stream m (Array Word8)    -- ^ Input stream
    -> Stream m (Array Word8)    -- ^ Output stream
pipeChunks = pipeChunksWith id

{-# DEPRECATED processChunks "Please use pipeChunks instead." #-}
{-# INLINE processChunks #-}
processChunks ::
    (MonadCatch m, MonadAsync m)
    => FilePath             -- ^ Executable name or path
    -> [String]             -- ^ Arguments
    -> Stream m (Array Word8)    -- ^ Input stream
    -> Stream m (Array Word8)    -- ^ Output stream
processChunks = pipeChunks

-- | Like 'pipeChunks' except that its input and output is stream of bytes
-- instead of a stream of chunks. The input to the pipe is buffered with a
-- buffer size of 'defaultChunkSize'.
--
-- For control over the input buffer use your own chunking and chunk based
-- APIs.
--
-- We can write the example in 'pipeChunks' as follows.
--
-- >>> :{
--    Process.toBytes "echo" ["hello world"]
--  & Process.pipeBytes "tr" ["[a-z]", "[A-Z]"]
--  & Stream.fold Stdio.write
--  :}
--HELLO WORLD
--
-- /pre-release/
{-# INLINE pipeBytes #-}
pipeBytes ::
    (MonadCatch m, MonadAsync m)
    => FilePath     -- ^ Executable name or path
    -> [String]     -- ^ Arguments
    -> Stream m Word8    -- ^ Input Stream
    -> Stream m Word8    -- ^ Output Stream
pipeBytes path args input = -- rights . pipeBytesEither path args
    let input1 = CHUNKS_OF defaultChunkSize input
        output = pipeChunks path args input1
     in UNFOLD_EACH Array.reader output

{-# DEPRECATED processBytes "Please use pipeBytes instead." #-}
{-# INLINE processBytes #-}
processBytes ::
    (MonadCatch m, MonadAsync m)
    => FilePath     -- ^ Executable name or path
    -> [String]     -- ^ Arguments
    -> Stream m Word8    -- ^ Input Stream
    -> Stream m Word8    -- ^ Output Stream
processBytes = pipeBytes

-- | Like 'pipeChunks' except that its input and output is stream of chars
-- instead of a stream of chunks. The input to the pipe is buffered with a
-- buffer size of 'defaultChunkSize'.
--
-- For control over the input buffer use your own chunking and chunk based
-- APIs.
--
-- NOTE: This API uses UTF-8 encoding.
--
-- >>> :{
--    Process.toChars "echo" ["hello world"]
--  & Process.pipeChars "tr" ["[a-z]", "[A-Z]"]
--  & Stdio.putChars
--  :}
--HELLO WORLD
--
-- We can seamlessly replace the @tr@ process with the Haskell @toUpper@
-- function:
--
-- >>> :{
--    Process.toChars "echo" ["hello world"]
--  & fmap toUpper
--  & Stdio.putChars
--  :}
--HELLO WORLD
--
-- /pre-release/
{-# INLINE pipeChars #-}
pipeChars ::
    (MonadCatch m, MonadAsync m)
    => FilePath     -- ^ Executable name or path
    -> [String]     -- ^ Arguments
    -> Stream m Char    -- ^ Input Stream
    -> Stream m Char    -- ^ Output Stream
pipeChars path args input =
    Unicode.encodeUtf8 input
        & pipeBytes path args
        & Unicode.decodeUtf8

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-- Note: It is allowed to inheritStdout or inheritStderr but not both as that
-- would not generate a stream for further processing and would result in unintuitive
-- behaviour

-- | Like 'toChunksEither' but use the specified configuration to run the
-- process.
{-# INLINE toChunksEitherWith #-}
toChunksEitherWith ::
    (MonadCatch m, MonadAsync m)
    => (Config -> Config)   -- ^ Config modifier
    -> FilePath             -- ^ Executable name or path
    -> [String]             -- ^ Arguments
    -> Stream m (Either (Array Word8) (Array Word8))     -- ^ Output stream
toChunksEitherWith modifier path args =
    pipeChunksWithAction run (modifier . inheritStdin . pipeStdErr . pipeStdout) path args

    where

    run (_, Nothing, Nothing, _) =
        error "toChunksEitherWith: only one of stdout or stderr can be inherited"
    run (_, stdoutMaybe, stderrMaybe, _) =
        fmap Left (whenJustS toChunksClose stderrMaybe)
            `parallel` fmap Right (whenJustS toChunksClose stdoutMaybe)

-- Note: inheritStdout has no affect, we always pipe stdout to the resulting
-- stream

-- | Like 'toChunks' but use the specified configuration to run the process.
{-# INLINE toChunksWith #-}
toChunksWith ::
    (MonadCatch m, MonadAsync m)
    => (Config -> Config)   -- ^ Config modifier
    -> FilePath             -- ^ Executable name or path
    -> [String]             -- ^ Arguments
    -> Stream m (Array Word8)    -- ^ Output stream
toChunksWith modifier path args =
    pipeChunksWithAction run (pipeStdout . modifier . inheritStdin) path args

    where

    run (_, Just stdoutH, _, _) = toChunksClose stdoutH
    run _                       = error "toChunksWith: Not reachable"

-- | @toBytesEither path args@ runs the executable at @path@ using @args@ as
-- arguments and returns the output of the process as a stream of 'Either'
-- bytes. The 'Left' values are from @stderr@ and the 'Right' values are from
-- @stdout@ of the executable.
--
-- Raises 'ProcessFailure' exception in case of failure.
--
-- The following example uses @echo@ to write @hello@ to @stdout@ and @world@
-- to @stderr@, then uses folds from "Streamly.Console.Stdio" to write them
-- back to @stdout@ and @stderr@ respectively:
--
-- >>> :{
--   Process.toBytesEither "/bin/bash" ["-c", "echo 'hello'; echo 'world' 1>&2"]
-- & Stream.fold (Fold.partition Stdio.writeErr Stdio.write)
-- :}
-- world
-- hello
-- ((),())
--
-- @since 0.1.0
{-# INLINE toBytesEither #-}
toBytesEither ::
    (MonadAsync m, MonadCatch m)
    => FilePath     -- ^ Executable name or path
    -> [String]     -- ^ Arguments
    -> Stream m (Either Word8 Word8)    -- ^ Output Stream
toBytesEither path args =
    let output = toChunksEither path args
        leftRdr = fmap Left Array.reader
        rightRdr = fmap Right Array.reader
     in UNFOLD_EACH (Unfold.either leftRdr rightRdr) output

-- | @toBytes path args@ runs the executable specified by @path@ using @args@
-- as arguments and returns the output of the process as a stream of bytes.
--
-- Raises 'ProcessFailure' exception in case of failure.
--
-- The following code is equivalent to the shell command @echo "hello world"@:
--
-- >>> :{
--    Process.toBytes "echo" ["hello world"]
--  & Stream.fold Stdio.write
--  :}
--hello world
--
-- @since 0.1.0
{-# INLINE toBytes #-}
toBytes ::
    (MonadAsync m, MonadCatch m)
    => FilePath     -- ^ Executable name or path
    -> [String]     -- ^ Arguments
    -> Stream m Word8    -- ^ Output Stream
toBytes path args =
    let output = toChunks path args
     in UNFOLD_EACH Array.reader output

-- | Like 'toBytesEither' but generates a stream of @Array Word8@ instead of a stream
-- of @Word8@.
--
-- >>> :{
--   toChunksEither "bash" ["-c", "echo 'hello'; echo 'world' 1>&2"]
-- & Stream.fold (Fold.partition Stdio.writeErrChunks Stdio.writeChunks)
-- :}
-- world
-- hello
-- ((),())
--
-- >>> toChunksEither = toChunksEitherWith id
--
-- Prefer 'toChunksEither' over 'toBytesEither' when performance matters.
--
-- /Pre-release/
{-# INLINE toChunksEither #-}
toChunksEither ::
    (MonadAsync m, MonadCatch m)
    => FilePath             -- ^ Executable name or path
    -> [String]             -- ^ Arguments
    -> Stream m (Either (Array Word8) (Array Word8))    -- ^ Output Stream
toChunksEither = toChunksEitherWith id

-- | @toChunks path args@ runs the executable specified by @path@ using @args@
-- as arguments and returns the output of the process as a stream of chunks of
-- bytes (Array Word8).
--
-- Raises 'ProcessFailure' exception in case of failure.
--
-- The following code is equivalent to the shell command @echo "hello world"@:
--
-- >>> :{
--    Process.toChunks "echo" ["hello world"]
--  & Stream.fold Stdio.writeChunks
--  :}
--hello world
--
-- >>> toChunks = toChunksWith id
--
-- @since 0.1.0
{-# INLINE toChunks #-}
toChunks ::
    (MonadAsync m, MonadCatch m)
    => FilePath             -- ^ Executable name or path
    -> [String]             -- ^ Arguments
    -> Stream m (Array Word8)    -- ^ Output Stream
toChunks = toChunksWith id

-- | @toChars path args@ runs the executable specified by @path@ using @args@
-- as arguments and returns the output of the process as a stream of chars.
--
-- NOTE: This API uses UTF-8 encoding.
--
-- Raises 'ProcessFailure' exception in case of failure.
--
-- Definition:
--
-- >>> toChars path args = toBytes path args & Unicode.decodeUtf8
--
{-# INLINE toChars #-}
toChars ::
    (MonadAsync m, MonadCatch m)
    => FilePath       -- ^ Executable name or path
    -> [String]       -- ^ Arguments
    -> Stream m Char -- ^ Output Stream
toChars path args = toBytes path args & Unicode.decodeUtf8

-- | @toLines f path args@ runs the executable specified by @path@ using @args@
-- as arguments and folds the output of the process at line breaks, using the
-- fold @f@, to return a stream of folded lines.
--
-- NOTE: This API uses UTF-8 encoding.
--
-- Raises 'ProcessFailure' exception in case of failure.
--
-- To return a stream of lines as strings:
--
-- >>> toStrings = toLines Fold.toList
--
-- Definition:
--
-- >>> toLines f path args = toChars path args & Unicode.lines f
--
{-# INLINE toLines #-}
toLines ::
    (MonadAsync m, MonadCatch m)
    => Fold m Char a
    -> FilePath       -- ^ Executable name or path
    -> [String]       -- ^ Arguments
    -> Stream m a -- ^ Output Stream
toLines f path args = toChars path args & Unicode.lines f

-- | @toString path args@ runs the executable specified by @path@ using @args@
-- as arguments and folds the entire output of the process as a single string.
--
-- NOTE: This API uses UTF-8 encoding.
--
-- Definition:
--
-- >>> toString path args = toChars path args & Stream.toList
--
{-# INLINE toString #-}
toString ::
    (MonadAsync m, MonadCatch m)
    => FilePath -- ^ Executable name or path
    -> [String] -- ^ Arguments
    -> m String
toString path args = toChars path args & Stream.toList

-- | @toStdout path args@ runs the executable specified by @path@ using @args@
-- as arguments and returns the output of the process on stdout.
--
-- Definition:
--
-- >>> toStdout path args = toChunks path args & Stdio.putChunks
--
{-# INLINE toStdout #-}
toStdout ::
    (MonadAsync m, MonadCatch m)
    => FilePath -- ^ Executable name or path
    -> [String] -- ^ Arguments
    -> m ()
toStdout path args = toChunks path args & Stdio.putChunks
{-
-- Directly inherits stdout instead.
toStdout path args = do
    _ <- liftIO $ createProc' (inheritStdin . inheritStdout) path args
    return ()
-}

-- | @toNull path args@ runs the executable specified by @path@ using @args@
-- as arguments and discards the output of the process.
--
-- Definition:
--
-- >>> toNull path args = toChunks path args & Stream.fold Fold.drain
--
{-# INLINE toNull #-}
toNull ::
    (MonadAsync m, MonadCatch m)
    => FilePath -- ^ Executable name or path
    -> [String] -- ^ Arguments
    -> m ()
toNull path args = toChunks path args & Stream.fold Fold.drain

-------------------------------------------------------------------------------
-- Processes not interacting with the parent process
-------------------------------------------------------------------------------

-- XXX Make the return type ExitCode/ProcessHandle depend on the wait argument?

-- | Launch a standalone process i.e. the process does not have a way to attach
-- the IO streams with other processes. The IO streams stdin, stdout, stderr
-- can either be inherited from the parent or closed.
--
-- This API is more powerful than 'interactive' and 'daemon' and can be used to
-- implement both of these. However, it should be used carefully e.g. if you
-- inherit the IO streams and parent is not waiting for the child process to
-- finish then both parent and child may use the IO streams resulting in
-- garbled IO if both are reading/writing simultaneously.
--
-- If the parent chooses to wait for the process an 'ExitCode' is returned
-- otherwise a 'ProcessHandle' is returned which can be used to terminate the
-- process, send signals to it or wait for it to finish.
{-# INLINE standalone #-}
standalone ::
       Bool -- ^ Wait for process to finish?
    -> (Bool, Bool, Bool) -- ^ close (stdin, stdout, stderr)
    -> (Config -> Config)
    -> FilePath -- ^ Executable name or path
    -> [String] -- ^ Arguments
    -> IO (Either ExitCode ProcessHandle)
standalone wait (close_stdin, close_stdout, close_stderr) modCfg path args =
    withCreateProcess cfg postCreate

    where

    postCreate _ _ _ procHandle =
        if wait
        then Left <$> waitForProcess procHandle
        else return $ Right procHandle

    cfg =
        let Config c = modCfg $ mkConfig path args
            s_in = if close_stdin then NoStream else Inherit
            s_out = if close_stdout then NoStream else Inherit
            s_err = if close_stderr then NoStream else Inherit
        in c {std_in = s_in, std_out = s_out, std_err = s_err}

-- | Launch a process interfacing with the user. User interrupts are sent to
-- the launched process and ignored by the parent process. The launched process
-- inherits stdin, stdout, and stderr from the parent, so that the user can
-- interact with the process. The parent waits for the child process to exit,
-- an 'ExitCode' is returned when the process finishes.
--
-- This is the same as the common @system@ function found in other libraries
-- used to execute commands.
--
-- On Windows you can pass @setSession NewConsole@ to create a new console.
--
{-# INLINE foreground #-}
foreground ::
       (Config -> Config)
    -> FilePath -- ^ Executable name or path
    -> [String] -- ^ Arguments
    -> IO ExitCode
foreground modCfg path args =
    let r =
            standalone
                True
                (False, False, False)
                (parentIgnoresInterrupt True . modCfg)
                path args
     in fmap (either id undefined) r

{-# DEPRECATED interactive "Use foreground instead." #-}
{-# INLINE interactive #-}
interactive ::
       (Config -> Config)
    -> FilePath -- ^ Executable name or path
    -> [String] -- ^ Arguments
    -> IO ExitCode
interactive = foreground

-- XXX ProcessHandle can be used to terminate the process. re-export
-- terminateProcess?

-- | Launch a daemon process. Closes stdin, stdout and stderr, creates a new
-- session, detached from the terminal, the parent does not wait for the
-- process to finish.
--
-- The 'ProcessHandle' returned can be used to terminate the daemon or send
-- signals to it.
--
{-# INLINE daemon #-}
daemon ::
       (Config -> Config)
    -> FilePath -- ^ Executable name or path
    -> [String] -- ^ Arguments
    -> IO ProcessHandle
daemon modCfg path args =
    let r =
            standalone
                False
                (True, True, True)
                (setSession NewSession . modCfg)
                path args
     in fmap (either undefined id) r


whenJustS :: Applicative m => (a -> Stream m b) -> Maybe a -> Stream m b
whenJustS action mb = maybe Stream.nil action mb
