{- $setup

>>> :set -XFlexibleContexts
>>> :set -XScopedTypeVariables
>>> :set -Wno-deprecations
>>> import Data.Char (toUpper)
>>> import Data.Function ((&))
>>> import qualified Streamly.Console.Stdio as Stdio
>>> import qualified Streamly.Data.Array as Array
>>> import qualified Streamly.Data.Fold as Fold
>>> import qualified Streamly.Data.Stream.Prelude as Stream
>>> import qualified Streamly.System.Process as Process
>>> import qualified Streamly.Unicode.Stream as Unicode

For APIs that have not been released yet.

>>> import Streamly.Internal.System.IO (defaultChunkSize)
>>> import qualified Streamly.Internal.Console.Stdio as Stdio (putChars, putChunks, readChunks)
>>> import qualified Streamly.Internal.FileSystem.Dir as Dir (readFiles)
>>> import qualified Streamly.Internal.System.Process as Process
>>> import qualified Streamly.Internal.Unicode.Stream as Unicode (lines)
-}
