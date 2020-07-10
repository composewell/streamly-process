module Streamly.System.Utility 
    (
        unquoteCommand
    )
where

import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as P
import qualified Streamly.Internal.Data.Parser.ParserD as D
import Streamly.Internal.Data.Parser (Parser(..))
import Streamly.Internal.Data.Parser.ParserD.Types (Step (..))

import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadCatch)
import Control.Exception (displayException)

{-# INLINE isEofD #-}
isEofD :: Monad m => D.Parser m a Bool
isEofD = D.Parser step initial return
  where
    initial = return True
    step _ _ = return $ Done 1 False

{-# INLINE isEof #-}
isEof :: MonadCatch m => Parser m a Bool
isEof = D.toParserK isEofD

{-# INLINE conv2list #-}
conv2list :: Monad m => Parser m a b -> Parser m a [b]
conv2list parser = fmap (\x -> [x]) parser

{-# INLINE singleQuote #-}
singleQuote :: MonadCatch m => Parser m Char [Char]
singleQuote = conv2list $ P.satisfy (=='\'')

{-# INLINE parseWithinSingle #-}
parseWithinSingle :: MonadCatch m => Parser m Char String
parseWithinSingle = do
    let failCond char = char /= '\\' && char /= '\''
    str <- P.takeWhile failCond FL.toList
    char <- P.peek
    if char == '\'' then
        return str
    else
        do
            escapedChar <- P.peek
            restStr <- parseWithinSingle
            return (str ++ [escapedChar] ++ restStr)

{-# INLINE parseSingleQuoteArg #-}
parseSingleQuoteArg :: MonadCatch m => Parser m Char String
parseSingleQuoteArg =
    concatThreeList <$> singleQuote <*> parseWithinSingle <*> singleQuote

    where concatThreeList xs ys zs = xs ++ ys ++ zs

{-# INLINE parseManySingleQuote #-}
parseManySingleQuote :: MonadCatch m => Parser m Char String
parseManySingleQuote = P.some FL.mconcat parseSingleQuoteArg

{-# INLINE doubleQuote #-}
doubleQuote :: MonadCatch m => Parser m Char [Char]
doubleQuote = conv2list $ P.satisfy (=='"')

{-# INLINE parseWithinDouble #-}
parseWithinDouble :: MonadCatch m => Parser m Char String
parseWithinDouble = do
    let noFailCond char = char /= '\\' && char /= '"'
    str <- P.takeWhile noFailCond FL.toList
    char <- P.peek
    if char == '"' then
        return str
    else
        do
            escapedChar <- P.peek
            restStr <- parseWithinDouble
            return (str ++ [escapedChar] ++ restStr)

{-# INLINE parseDoubleQuoteArg #-}
parseDoubleQuoteArg :: MonadCatch m => Parser m Char String
parseDoubleQuoteArg =
    concatThreeList <$> doubleQuote <*> parseWithinDouble <*> doubleQuote

    where concatThreeList xs ys zs = xs ++ ys ++ zs

{-# INLINE parseManyDoubleQuote #-}
parseManyDoubleQuote :: MonadCatch m => Parser m Char String
parseManyDoubleQuote = P.some FL.mconcat parseDoubleQuoteArg

{-# INLINE parseNoQuote #-}
parseNoQuote :: MonadCatch m => Parser m Char String
parseNoQuote = do
    let noFailCond char = char /= '\\' && char /= '"' && char /= ' '
    str <- P.takeWhile noFailCond FL.toList
    eof <- isEof
    if eof then
        return str
    else
        do
            failChar <- P.peek
            if failChar == ' ' then
                return str
            else
                P.die "Command Parse Error"

{-# INLINE parseArgument #-}
parseArgument :: MonadCatch m => Parser m Char String
parseArgument = parseManySingleQuote <|> parseManyDoubleQuote <|> parseNoQuote

{-# INLINE parseCommand #-}
parseCommand :: MonadCatch m => Parser m Char (String, [String])
parseCommand = do
    let 
        parseArgWithSpace = do
            arg <- parseArgument
            P.takeWhile (== ' ') FL.drain
            return arg

        parseAllArgs = P.many FL.toList parseArgWithSpace

    execName <- P.takeWhile (/= ' ') FL.toList
    P.takeWhile (== ' ') FL.drain
    argList <- parseAllArgs
    P.eof
    return (execName, argList)

unquoteCommand :: String -> (String, [String])
unquoteCommand command =
    case S.parse parseCommand (S.fromList command) of
        Left e -> error $ displayException e
        Right parsedCmd -> parsedCmd
