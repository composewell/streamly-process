-- |
-- Module      : Streamly.System.Process
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides functions to turn operating system processes into
-- stream source, sink or transformation functions. Thus OS processes can be
-- used like regular Haskell stream functions connecting them into a stream
-- pipeline consisting of Haskell functions or other OS processes.
--
-- Processes can be composed in a streaming pipeline just like a Posix shell
-- command pipeline except that we use @&@ instead of @|@. Also note that like
-- the @pipefail@ option in shells, exceptions are propagated if any of the
-- stages fail.
--
-- The default attributes of the new process created by the APIs in this module
-- are described below:
--
-- * The following attributes are inherited from the parent:
--
--     * Current working directory
--     * Environment
--     * Process group
--     * Process uid and gid
--     * Signal handlers
--     * Terminal (Session)
--
-- * All fds except stdin, stdout and stderr are closed in the child

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
