module Main (main) where

import Data.Char (toUpper)
import Data.Function ((&))
import System.Environment (getArgs)

import qualified Streamly.Prelude as Stream
import qualified Streamly.Console.Stdio as Stdio
import qualified Streamly.Unicode.Stream as Unicode

main :: IO ()
main = do
    args <- getArgs
    let f =
            case args of
                x:_ ->
                    case x of
                        "stdout" -> Stdio.write
                        "stderr" -> Stdio.writeErr
                        _ -> error "Invalid arg"
                _ -> error "Usage: exe stdout|stderr"

    Stream.unfold Stdio.read ()
        & Unicode.decodeLatin1
        & Stream.map toUpper
        & Unicode.encodeLatin1
        & Stream.fold f
