{- $setup

>>> :m
>>> :set -XFlexibleContexts
>>> :set -XQuasiQuotes
>>> import Data.Char (toUpper)
>>> import Data.Function ((&))
>>> import Streamly.Unicode.String (str)
>>> import qualified Streamly.Data.Array as Array
>>> import qualified Streamly.Console.Stdio as Stdio
>>> import qualified Streamly.Data.Fold as Fold
>>> import qualified Streamly.Data.Stream.Prelude as Stream
>>> import qualified Streamly.System.Command as Command
>>> import qualified Streamly.Unicode.Stream as Unicode

>>> import qualified Streamly.Internal.System.Process as Process
>>> import qualified Streamly.Internal.Console.Stdio as Stdio
>>> import qualified Streamly.Internal.FileSystem.Dir as Dir
-}
