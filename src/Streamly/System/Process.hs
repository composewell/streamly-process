-- |
-- Module      : Streamly.System.Process
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides functions to run operating system processes as stream
-- source, sink or transformation functions. Thus OS processes can be used in
-- the same way as Haskell functions and all the streaming combinators in
-- streamly can be used to combine them. This allows you to seamlessly
-- integrate external programs into your Haskell program.
--
-- We recommend that you use Streamly threads instead of system processes where
-- possible as they have a simpler programming model and processes have a
-- larger performance overhead.
--
-- = Process Attributes
--
-- As usual, in the new process the following attributes are inherited from the
-- parent process:
--
--     * Current working directory
--     * Environment
--     * Open file descriptors
--     * Process group
--     * Process uid and gid
--     * Signal handlers
--     * Terminal (Session)
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
