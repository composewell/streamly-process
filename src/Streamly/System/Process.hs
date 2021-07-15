-- |
-- Module      : Streamly.System.Process
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides functions to run operating system processes as stream
-- source, sink or transformation functions. Thus OS processes can be used like
-- regular Haskell stream functions and all the streaming combinators in
-- streamly can be used to combine them.
--
-- = Process Attributes
--
-- The default attributes of the new process created by the APIs in this module
-- are described below:
--
-- * The following attributes are inherited from the parent process:
--
--     * Current working directory
--     * Environment
--     * Open file descriptors
--     * Process group
--     * Process uid and gid
--     * Signal handlers
--     * Terminal (Session)
--
-- See the documentation of specific functions for the behavior of @stdin@,
-- @stdout@ and @stderr@ file descriptors.
--
-- = Shell Programming
--
-- Streamly provides powerful ways to combine streams. Processes can be
-- composed in a streaming pipeline just like a Posix shell command pipeline
-- except that we use @&@ instead of @|@. Also note that like the @pipefail@
-- option in shells, exceptions are propagated if any of the stages fail.
--

module Streamly.System.Process
    ( ProcessFailure (..)

    -- * Generation
    , toBytes
    , toChunks

    -- * Transformation
    , processBytes
    , processChunks
    )
where

import Streamly.Internal.System.Process
