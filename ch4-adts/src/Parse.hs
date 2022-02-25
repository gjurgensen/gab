-- {-# LANGUAGE FlexibleContexts #-}

module Parse where

import qualified Data.Map.Strict as Map
import Data.List.NonEmpty
import Text.Parsec
import Text.Parsec.String
import Data.Char

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
keywords = ["if", "then", "else", "fn", "fix", "adt"]

ident :: Parser Ident
ident = tok (try $ do 
    c  <- lower
    cs <- many identChar
    let str = c:cs
    if str `elem` keywords then
        unexpected "keyword"
    else pure str
    ) <?> "identifier"

tvar :: Parser Type
tvar = TVar <$> ident

digitInt :: Parser Int
digitInt = digitToInt <$> digit

nat :: Parser Int
nat = foldl ((+) . (10*)) 0 <$> many1 digitInt

tunif :: Parser Type
tunif = fmap TUnif $ char '?' >> nat

typ :: Parser Type
typ = do
    t <- parens typ <|> tvar
    tarrow t <|> pure t
    <?> "type"
  where
    tarrow t = do
        symbol "->"
        TArr t <$> typ

var :: Parser Term
var = Var <$> ident

binding :: Parser (Ident, Type)
binding = do 
    i <- ident
    symbol ":"
    t <- typ
    pure (i,t)

lambda :: Parser Term
lambda = tok ( do 
    symbol "λ" <|> keyword "fn"
    -- args <- choice [pure <$> binding, many1 $ parens binding]
    args <- many1 ident
    symbol "."
    body <- term
    pure $ foldr (Lambda Map.empty) body args
    ) <?> "lambda expression"

fix :: Parser Term
fix = do 
    keyword "fix"
    Fix <$> term

term :: Parser Term
term = do 
    hd <- subterm 
    tl <- many subterm
    pure $ foldl App hd tl
    <?> "term"
  where
    terms   = many1 subterm
    subterm = choice [parens term, lambda, fix, var]

bindStmt :: Parser Stmt
bindStmt = do
    i <- ident
    symbol ":="
    Bind i <$> term

many1Ne :: Parser a -> Parser (NonEmpty a)
many1Ne p = do
    x  <- p
    xs <- many p
    pure $ x :| xs

sepBy1Ne :: Parser a -> Parser b -> Parser (NonEmpty a)
sepBy1Ne p delim = do
    x <- p
    delim
    xs <- sepBy p delim
    pure $ x :| xs

adt :: Parser Stmt
adt = do
    keyword "data"
    name <- ident
    vars <- many ident
    symbol ":="
    cons <- sepBy1Ne constructor $ symbol "|"
    pure $ Adt name vars cons
    <?> "ADT"
  where
    constructor = (,) <$> ident <*> many typ

statement :: Parser Stmt
statement = bindStmt <|> adt <?> "statement"

program :: Parser [Stmt]
program = many $ statement <* symbol ";"

full :: Parser a -> Parser a
full p = spaces >> (p <* eof)