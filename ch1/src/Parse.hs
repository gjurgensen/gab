-- {-# LANGUAGE FlexibleContexts #-}

module Parse where

import qualified Data.Map.Strict as Map

import Text.Parsec
import Text.Parsec.String

import Ast


tok :: Parser a -> Parser a
tok = (<* spaces)

notBefore :: Show b => Parser a -> Parser b -> Parser a
p `notBefore` q = p <* notFollowedBy q

symbol :: String -> Parser String
symbol = tok . try . string

keyword :: String -> Parser String
-- Keyword is a symbol which is ambigious with identifiers
keyword s = tok $ try $ string s `notBefore` identChar

symbols :: [String] -> Parser String
symbols = choice . fmap symbol

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identChar :: Parser Char
identChar = alphaNum <|> oneOf ['-', '_', '\'']

keywords :: [String]
keywords = ["fn"]

ident :: Parser Ident
ident = tok (try $ do 
    c  <- lower
    cs <- many identChar
    let str = c:cs
    if str `elem` keywords then
        unexpected "keyword"
    else pure str
    ) <?> "identifier"

var :: Parser Term
var = Var <$> ident

lambda :: Parser Term
lambda = tok ( do 
    symbol "λ" <|> keyword "fn"
    args <- many1 ident
    symbol "."
    body <- term
    pure $ foldr (Lambda Map.empty) body args
    ) <?> "lambda expression"

term :: Parser Term
term = do 
    hd <- subterm 
    tl <- many subterm
    pure $ foldl App hd tl
    <?> "term"
  where
    subterm = parens term <|> lambda <|> var

parser :: Parser Term
parser = do
    spaces
    term <* eof