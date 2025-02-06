{-# LANGUAGE FlexibleContexts #-}

module Streamly.Diagram.System.Process (main) where

import Diagrams.Prelude
import Diagrams.Backend.SVG

rectWith :: String -> Double -> Double -> Diagram B
rectWith label x y =
    centerXY (text label # fontSizeL 0.3 <> rect x y # lw thick)

rightArrow :: Double -> Diagram B
rightArrow len = arrowV (r2 (len, 0)) # lw thick

withRightArrow :: Diagram B -> Diagram B -> Diagram B
withRightArrow a b = a ||| rightArrow 1.5 ||| b

upArrow :: Diagram B
upArrow = arrowV (1.5 *^ unit_Y # negate) # lw thick

withUpArrow :: Diagram B -> Diagram B -> Diagram B
withUpArrow a b = a === upArrow === b

downArrow :: Diagram B
downArrow = arrowV (1.5 *^ unit_Y) # lw thick

withDownArrow :: Diagram B -> Diagram B -> Diagram B
withDownArrow a b = a === downArrow === b

outPrefix :: String
outPrefix = "diagrams/img/System.Process."

main :: IO ()
main =
    mapM_
        (\(name, dia) -> renderSVG (outPrefix ++ name ++ ".svg")
        (mkWidth 200) dia) diagrams

    where

    diagrams =
            [ ("pipeChunksWith", pipeChunksWith)
            , ("toChunksWith", toChunksWith)
            , ("pipeChunksEitherWith.inheritStdout"
              , pipeChunksEitherWithInheritStdout
              )
            ]

    inputStream  = rectWith "input stream" 2 1
    stdinBox     = rectWith "stdin" 1 1
    childProcess = rectWith "child process" 2 1
    stdoutBox    = rectWith "stdout" 1 1
    outputStream = rectWith "output stream" 2 1

    pipeChunksWith =
        hcat
            [ withRightArrow inputStream stdinBox
            , childProcess
            , withRightArrow stdoutBox outputStream
            ]

    parentStdin = rectWith "parent stdin" 2 1
    toChunksWith =
            withUpArrow stdinBox parentStdin
            <> translateX 1.5 childProcess
            <> translateX 3 stdoutBox
            <> translateX 3.5 (rightArrow 1.5)
            <> translateX 6 (rectWith "output stream" 2 1)

    parentStdout = rectWith "parent stdout" 2 1
    pipeChunksEitherWithInheritStdout =
            withUpArrow stdinBox parentStdin
            <> translateX 1.5 childProcess
            <> translateX 3 (withDownArrow stdoutBox parentStdout)
            <> translateX 3.5 (rightArrow 1.5)
            <> translateX 6 (rectWith "Nil stream" 2 1)
