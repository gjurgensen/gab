-- {-# LANGUAGE FlexibleContexts #-}

module Parse where

import qualified Data.Map.Strict as Map

import Text.Parsec
import Text.Parsec.String

import Ast

-- keywords :: [String]
-- keywords = ["fn"]

tok :: Parser a -> Parser a
tok p = do
    a <- p
    spaces
    pure a

symbol :: String -> Parser String
symbol = tok . string

symbols :: [String] -> Parser String
symbols = choice . fmap symbol

ident :: Parser String
ident = tok ( do 
    c  <- lower
    cs <- many $ alphaNum <|> oneOf ['-', '_', '\'']
    pure $ c:cs
    ) <?> "identifier"

var :: Parser Term
var = Var <$> ident

followedBy :: Parser a -> Parser b -> Parser a
p `followedBy` q = do 
    x <- p
    q 
    pure x

lambda :: Parser Term
lambda = tok ( do 
    symbol "λ" <|> (string "fn" `followedBy` many1 space)
    args <- many1 ident
    symbol "."
    body <- term
    pure $ foldr (Lambda Map.empty) body args
    ) <?> "lambda expression"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

term :: Parser Term
term = do 
    hd <- subterm 
    tl <- many subterm
    pure $ foldl App hd tl
    <?> "term"
  where
    terms   = many1 subterm
    subterm = parens term <|> lambda <|> var

parser :: Parser Term
parser = do
    spaces
    term `followedBy` eof