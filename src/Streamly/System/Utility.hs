module Streamly.System.Utility 
    (
        escapeCommand
    )
where

import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser.ParserD as P
import Streamly.Internal.Data.Parser.ParserD (Parser(..))
import Streamly.Internal.Data.Parser.ParserD.Types (Step (..))

import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadCatch)
import Control.Exception (displayException)

isEof :: Monad m => Parser m a Bool
isEof = Parser step initial return
  where
    initial = return True
    step _ _ = return $ Done 1 False

conv2list :: Monad m => Parser m a b -> Parser m a [b]
conv2list parser = fmap (\x -> [x]) parser

singleQuote :: MonadCatch m => Parser m Char [Char]
singleQuote = conv2list $ P.satisfy (=='\'')

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

parseSingleQuoteArg :: MonadCatch m => Parser m Char String
parseSingleQuoteArg =
    concatThreeList <$> singleQuote <*> parseWithinSingle <*> singleQuote

    where concatThreeList xs ys zs = xs ++ ys ++ zs

parseManySingleQuote :: MonadCatch m => Parser m Char String
parseManySingleQuote = P.some FL.mconcat parseSingleQuoteArg

doubleQuote :: MonadCatch m => Parser m Char [Char]
doubleQuote = conv2list $ P.satisfy (=='"')

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

parseDoubleQuoteArg :: MonadCatch m => Parser m Char String
parseDoubleQuoteArg =
    concatThreeList <$> doubleQuote <*> parseWithinDouble <*> doubleQuote

    where concatThreeList xs ys zs = xs ++ ys ++ zs

parseManyDoubleQuote :: MonadCatch m => Parser m Char String
parseManyDoubleQuote = P.some FL.mconcat parseDoubleQuoteArg

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

parseArgument :: MonadCatch m => Parser m Char String
parseArgument = parseManySingleQuote <|> parseManyDoubleQuote <|> parseNoQuote

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

escapeCommand :: String -> (String, [String])
escapeCommand command =
    case S.parseD parseCommand (S.fromList command) of
        Left e -> error $ displayException e
        Right parsedCmd -> parsedCmd
