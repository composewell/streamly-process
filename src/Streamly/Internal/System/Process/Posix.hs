-- {-# LANGUAGE Safe #-}
-- |
-- Module      : Streamly.Internal.System.Process.Posix
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streamly.Internal.System.Process.Posix
    (
    -- * Processes
      Process
    , newProcess
    , wait
    , getStatus
    , terminate

    -- * IPC
    , mkPipe
    , mkStdioPipes
    )
where

import Control.Concurrent
    (MVar, newMVar, readMVar, withMVar, modifyMVar, modifyMVar_)
import Control.Exception (catch, throwIO, Exception(..), onException)
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Tuple (swap)
import GHC.IO.Device (IODeviceType(..))
import GHC.IO.Encoding (getLocaleEncoding)
import GHC.IO.Handle.FD (mkHandleFromFD)
import System.IO (IOMode(..), Handle, hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Posix.IO (createPipe, dupTo, closeFd)
import System.Posix.Process (forkProcess, executeFile, ProcessStatus, getProcessID)
import System.Posix.Types (ProcessID, Fd(..), CDev, CIno)
import System.Posix.Signals (signalProcess, sigTERM)
import System.Posix.Internals (fdGetMode)
import qualified Streamly.Internal.FileSystem.Dir as Dir
import qualified Streamly.Prelude as Stream

import qualified GHC.IO.FD as FD
import qualified System.Posix.Process as Posix
import Data.List (intercalate)

-------------------------------------------------------------------------------
-- Utilities to create stdio handles
-------------------------------------------------------------------------------

-- See GHC.IO.Handle.FD
-- We have to put the FDs into binary mode on Windows to avoid the newline
-- translation that the CRT IO library does.
setBinaryMode :: FD.FD -> IO ()





setBinaryMode _ = return ()


-- See Posix.fdToHandle and GHC.IO.Handle.FD.fdToHandle
-- See stdin, stdout, stderr in module GHC.IO.Handle.FD
--
-- This routines avoids a few system calls and does a few more things compared
-- to fdToHandle.
stdioFdToHandle ::
    Bool -> Maybe IOMode -> Maybe (IODeviceType, CDev, CIno) -> Fd -> IO Handle
stdioFdToHandle binary mbIOMode mbStat (Fd fdint) = do
    iomode <-
        case mbIOMode of
            Just mode -> return mode
            Nothing -> fdGetMode fdint
    (fd, fd_type) <- FD.mkFD fdint iomode mbStat
            False{-is_socket-}
              -- NB. the is_socket flag is False, meaning that:
              -- on Windows we're guessing this is not a socket (XXX)
            False{-is_nonblock-}
              -- file descriptors that we get from external sources are
              -- not put into non-blocking mode, because that would affect
              -- other users of the file descriptor
    setBinaryMode fd
    enc <- if binary then return Nothing else fmap Just getLocaleEncoding
    -- Should we set the FD non-blocking?
    -- See https://gitlab.haskell.org/ghc/ghc/-/issues/3316
    let fd_str = "<stdioFdToHandle file descriptor: " ++ show fd ++ ">"
    mkHandleFromFD fd fd_type fd_str iomode True{-non-block-} enc

-------------------------------------------------------------------------------
-- Setup pipes between parent and child processes
-------------------------------------------------------------------------------

-- | ParentToChild: parent writes, child reads
data Direction = ParentToChild | ChildToParent deriving (Show, Eq)

-- | return (parent, child, (parentAction, childAction, failureAction))
mkPipe :: Direction -> IO (Fd, Fd, (IO (), IO (), IO ()))
mkPipe direction = do
    let setDirection = if direction == ParentToChild then id else swap
    (child, parent) <- fmap setDirection createPipe
    let parentAction = closeFd child
        childAction = closeFd parent
        failureAction = closeFd child >> closeFd parent
    return (parent, child, (parentAction, childAction, failureAction))

-- | The child end of the pipe is duped to the supplied fd. The parent side fd
-- of the pipe is returned.
mkPipeDupChild :: Direction -> Fd -> IO (Fd, (IO (), IO (), IO ()))
mkPipeDupChild direction childFd = do
    let setDirection = if direction == ParentToChild then id else swap
    (child, parent) <- fmap setDirection createPipe
    pid <- getProcessID
    let parentAction = closeFd child
        childAction =
            hPutStrLn stderr ("closing parent fd" ++ show parent) >>
            closeFd parent >>
            hPutStrLn stderr ("closed parent fd" ++ show parent) >>
            hPutStrLn stderr ("duplicating child to fd" ++ show (child, childFd)) >>
            void (dupTo child childFd) >>
            hPutStrLn stderr ("duplicated child to fd" ++ show (child, childFd)) >>
            hPutStrLn stderr ("closing child" ++ show child) >>
            closeFd child >>
            hPutStrLn stderr ("closed child" ++ show child)
        failureAction = closeFd child >> closeFd parent
    return (parent, (parentAction, childAction, failureAction))

-- XXX We could possibly combine the triples from individual pipes in a more
-- idiomatic manner.
mkStdioPipes :: Bool -> IO ((Handle, Handle, Maybe Handle, Handle, Handle), IO (), IO (), IO ())
mkStdioPipes pipeStdErr = do
    -- stdin
    (inp, (inpParent, inpChild, inpFail)) <- mkPipeDupChild ParentToChild 0

    -- stdout
    (out, (outParent, outChild, outFail)) <- mkPipeDupChild ChildToParent 1
        `onException` inpFail

    -- stderr
    (err, (errParent, errChild, errFail)) <-
        if pipeStdErr
        then first Just <$> mkPipeDupChild ChildToParent 2
                `onException` (inpFail >> outFail)
        else return (Nothing, (return (), return (), return ()))

    {-
    -- exception channel
    (efdParent, efdChild, (excParent, excChild, excFail)) <-
        mkPipe ChildToParent
            `onException` (inpFail >> outFail >> errFail)
    -}

    let parentAction = inpParent >> outParent >> errParent -- >> excParent
        childAction =
            hPutStrLn stderr "child input action doing"
            >> inpChild
            >> hPutStrLn stderr "child input action done"
            >> hPutStrLn stderr "child output action doing"
            >> outChild
            >> hPutStrLn stderr "child output action done"
            >> errChild -- >> excChild
        -- childAction = inpChild >> outChild >> errChild -- >> excChild
        failureAction = inpFail >> outFail >> errFail -- >> excFail

    inpH <- toHandle WriteMode inp
    outH <- toHandle ReadMode out
    errH <-
        case err of
            Just x -> Just <$> toHandle ReadMode x
            Nothing -> return Nothing
    -- ehParent <- stdioFdToHandle
    --                  True (Just ReadMode) (Just (Stream, 0, 0)) efdParent
    -- ehChild Paren<- stdioFdToHandle
    --                  True (Just ReadMode) (Just (Stream, 0, 0)) efdChild
    let ehParent = undefined
    let ehChild = undefined
    return ( (inpH, outH, errH, ehParent, ehChild)
           , parentAction
           , childAction
           , failureAction
           )

    where

    toHandle mode = stdioFdToHandle False (Just mode) (Just (Stream, 0, 0))

-------------------------------------------------------------------------------
-- Process handle
-------------------------------------------------------------------------------

-- Note: We have two types of users reading and modifying the process handle,
-- (1) blocking waiters - wait, (2) non-blocking users -
-- getStatus. We need to ensure that blocking waiters always get the
-- status, and non-blocking waiters always get the status if the process is
-- done otherwise return Nothing.
--
-- One locking strategy could be that blocking waiters take a lock, and
-- non-blocking waiters try the same lock and if they cannot acquire it then
-- return Nothing. But the problem with this is that even after the process is
-- done non-blocking waiters locking may fail due to contention and it may
-- return Nothing, which is wrong.
--
-- Instead we use the following strategy.  Everyone first looks up the status
-- under the ProcessStatus lock. If the process is done we return the status.
-- If not done then:
--
-- - blocking users take the "waitlock" and perform a blocking waitpid, then
-- update the status by taking the ProcessStatus lock. This ensures that all
-- the blocking waiters are synchronized.
--
-- - non-blocking users perform non-blocking waitpid under the ProcessStatus
-- lock, this ensures that blocking users will not miss a reaping done by
-- non-blocking users. But non-blocking users may still miss a reaping done by
-- blocking users, if a blocking user reaped but is waiting for ProcessStatus
-- lock to update it. To take care of that case non-blocking users use the
-- result of waitpid to detect if the process has already been reaped and if so
-- they try again using the blocking users' waitlock. We know that this cannot
-- block for a long time any more since the process has been reaped. This time
-- if we still cannot get the status then it is some real error or bug so we
-- raise an exception.
--
-- | Thread safe, mutable process handle. Process status is stored in the
-- handle and is modified by the process inspection operations.
data Process =
    Process
        ProcessID -- Read only
        (MVar ()) -- waitlock
        (MVar (Maybe ProcessStatus)) -- ProcessStatus lock

-------------------------------------------------------------------------------
-- Creating a Process
-------------------------------------------------------------------------------

-- If this API is to be exported then we should wrap it in another function
-- that checks if the pid really exists by doing a non-blocking waitpid on it.
--
-- | Turn an existing process pid into a 'Process' handle.
pidToProcess :: ProcessID -> Maybe ProcessStatus -> IO Process
pidToProcess pid status = do
    waitLock <- newMVar ()
    st <- newMVar status
    return $ Process pid waitLock st

-- | Creates a new process, executes the specified action in the cloned process
-- and then performs an @exec@ system call using the provided path, arguments
-- and environment. The PATH is searched for the specified binary when the
-- specified path is not absolute?
newProcess ::
       IO ()                    -- ^ Execute after fork, before exec
    -> FilePath                 -- ^ Executable path
    -> [String]                 -- ^ Arguments
    -> Maybe [(String, String)] -- ^ Environment
    -> IO Process
newProcess action path args env = do
    pid <- forkProcess exec
    hPutStrLn stderr ("parent process " ++ show pid)
    pidToProcess pid Nothing

    where

    -- XXX What if exec failed or the "action" failed? Need to send the error
    -- to the parent and clean up the parent fds. We can send the exceptions
    -- via a pipe like we do for threads.
    --
    exec = do
        pid <- getProcessID
        hPutStrLn stderr ("child process " ++ show pid)
        fds <- Stream.toList . Stream.unfold Dir.readFiles $ ("/proc/" ++ show pid ++ "/fd")
        hPutStrLn stderr (intercalate ", " fds)
        action >> executeFile path True args env

newtype ProcessDoesNotExist = ProcessDoesNotExist ProcessID deriving Show

instance Exception ProcessDoesNotExist where

    displayException (ProcessDoesNotExist pid) =
        "Bug: Process with pid " ++ show pid ++ " does not exist."

-- | Wait until the process exits by itself or gets terminated due to a signal.
-- Returns the 'ProcessStatus' which includes the termination reason or exit
-- code.
--
-- Thread safe.
wait :: Process -> IO ProcessStatus
wait (Process pid waitLock procStatus) = do
    status <- readMVar procStatus
    case status of
        Just st -> return st
        Nothing -> withMVar waitLock $ \() -> waitStatus `catch` eChild

    where

    waitStatus = do
        st <- Posix.getProcessStatus True{-block-} False{-stopped-} pid
        case st of
            Nothing -> error $ "wait: Bug: Posix.getProcessStatus "
                ++ "returned Nothing in blocking mode"
            Just s -> do
                modifyMVar_ procStatus $ \_ -> return st
                return s

    -- Got eCHILD, some non-blocking user already reaped the process.
    eChild e = do
        if isDoesNotExistError e
        then do
            st <- readMVar procStatus
            case st of
                Nothing -> throwIO $ ProcessDoesNotExist pid
                Just s -> return s
        else throwIO e

-- | Get the current status of a process. A 'Nothing' value means the process
-- is still running, a 'Just' value means the process is terminated and
-- provides the status of the process.
--
-- Thread safe.
--
getStatus :: Process -> IO (Maybe ProcessStatus)
getStatus proc@(Process pid _ procStatus) = do
    r <- modifyMVar
            procStatus
            $ \old ->
                case old of
                    Just _ -> return (old, Just old)
                    Nothing -> fetchStatus `catch` eChild
    case r of
        Just st -> return st
        Nothing -> Just <$> wait proc

    where

    fetchStatus = do
        st <- Posix.getProcessStatus False{-block-} False{-stopped-} pid
        return (st, Just st)

    -- Got eCHILD, some blocking user already reaped the process.
    -- We need to go through the blocking wait API to synchronize.
    eChild e = do
        if isDoesNotExistError e
        then return (Nothing, Nothing)
        else throwIO e

terminate :: Process -> IO ()
terminate (Process pid _ _) = signalProcess sigTERM pid
