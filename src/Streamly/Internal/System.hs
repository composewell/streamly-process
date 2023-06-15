-- |
-- Module      : Streamly.Internal.System
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
{-# LANGUAGE TemplateHaskell #-}

module Streamly.Internal.System
(cmd)
where

import Control.Applicative (Alternative(..))
import Control.Exception (displayException)
import Data.Functor.Identity (runIdentity)
import Streamly.Internal.Data.Parser (Parser)

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
    (some, many, takeWhile1)
import qualified Streamly.Data.Stream as Stream  (fromList, parse)
import qualified Streamly.Internal.Unicode.Parser as Parser

data StrSegment
    = StrText String
    | StrVar String
    deriving (Show, Eq)

formatSpace :: String -> String
formatSpace = foldr go ""
  where
    go x acc = x:if x == ' ' then dropWhile (' ' ==) acc else acc

-- | Replace a newline by a space and convert multiple spaces to single space
--
-- >>> trim "  abc   \n   bbb  \n  ccc  "
-- "abc bbb ccc"
--
trim :: String -> String
trim = formatSpace <$> (unwords . fmap formatSpace . lines)

haskellIdentifier :: Monad m => Parser Char m String
haskellIdentifier =
    let p = Parser.alphaNum <|> Parser.char '\'' <|> Parser.char '_'
     in Parser.some p Fold.toList

strParser :: Monad m => Parser Char m [StrSegment]
strParser = Parser.many content Fold.toList

    where

    plainText = StrText . trim <$> Parser.takeWhile1 (/= '#') Fold.toList
    escHash = StrText . (: []) <$> (Parser.char '#' *> Parser.char '#')
    lineCont = StrText [] <$ (Parser.char '#' *> Parser.char '\n')
    var = StrVar <$>
            (  Parser.char '#'
            *> Parser.char '{'
            *> haskellIdentifier
            <* Parser.char '}'
            )
    plainHash = StrText . (: []) <$> Parser.char '#'

    -- order is important
    content = plainText <|> escHash <|> lineCont <|> var <|> plainHash

strSegmentExp :: StrSegment -> Q Exp
strSegmentExp (StrText text) = stringE text
strSegmentExp (StrVar name) = do
    valueName <- lookupValueName name
    case valueName of
        Just vn -> varE vn
        Nothing ->
            fail
                $ "cmd quote: Haskell symbol `" ++ name
                ++ "` is not in scope"

strExp :: [StrSegment] -> Q Exp
strExp xs = appE [| concat |] $ listE $ map strSegmentExp xs

expandVars :: String -> Q Exp
expandVars ln =
    case runIdentity $ Stream.parse strParser (Stream.fromList ln) of
        Left e ->
            fail $ "cmd QuasiQuoter parse error: " ++ displayException e
        Right x ->
            strExp x

-- | A QuasiQuoter that treats the input as a string literal:
--
-- >>> [cmd|x|]
-- "x"
--
-- Any @#{symbol}@ is replaced by the value of the Haskell symbol @symbol@
-- which is in scope:
--
-- >>> x = "hello"
-- >>> [cmd|#{x} world!|]
-- "hello world!"
--
-- @##@ means a literal @#@ without the special meaning for referencing
-- haskell symbols:
--
-- >>> [cmd|##{x} world!|]
-- "#{x} world!"
--
-- A @#@ at the end of line means the line continues to the next line without
-- introducing a newline character:
--
-- >>> :{
-- [cmd|hello#
-- world!|]
-- :}
-- "hello world!"
--
-- Bugs: because of a bug in parsers, a lone # at the end of input gets
-- removed.
--
cmd :: QuasiQuoter
cmd =
    QuasiQuoter
        { quoteExp = expandVars
        , quotePat = notSupported
        , quoteType = notSupported
        , quoteDec = notSupported
        }

    where

    notSupported = error "cmd: Not supported."
