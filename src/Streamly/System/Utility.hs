module Streamly.System.Utility 
    (
        escapeCommand
    )
where

import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser.ParserD as P
import Streamly.Internal.Data.Parser.ParserD (Parser)

import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Identity (Identity(..))

singleQuote :: MonadCatch m => Parser m Char [Char]
singleQuote = fmap (\x -> [x]) $ P.satisfy (=='\'')

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
doubleQuote = fmap (\x -> [x]) $ P.satisfy (=='"')

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
    let noFailCond char = char /= '\\' && char /= '"'
    str <- P.takeWhile noFailCond FL.toList
    P.eof
    return str

parseArgument :: MonadCatch m => Parser m Char String
parseArgument = parseManySingleQuote <|> parseManyDoubleQuote <|> parseNoQuote

escapeArgument :: String -> String
escapeArgument arg =
    case S.parseD parseArgument (S.fromList arg) of
        Right str -> str
        Left _ -> error "Command Parse Error"

splitOnSpace :: String -> [String]
splitOnSpace ls =
    case S.toList $ S.splitOn (==' ') FL.toList (S.fromList ls) of
        Identity splitLs -> splitLs

escapeCommand :: String -> (String, [String])
escapeCommand command = case splitOnSpace command of
    [] -> error "No Executable"
    (execFilePath : unEscapedArgList) -> (execFilePath, map escapeArgument unEscapedArgList)