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
keywords = ["if", "then", "else", "fn", "fix"]

ident :: Parser Ident
ident = tok (try $ do 
    c  <- lower
    cs <- many identChar
    let str = c:cs
    if str `elem` keywords then
        unexpected "keyword"
    else pure str
    ) <?> "identifier"

tbool :: Parser Type
tbool = TBool <$ symbol "Bool"

typ :: Parser Type
typ = do
    t <- parens typ <|> tbool
    tarrow t <|> pure t
    <?> "type"
  where
    tarrow t = do
        symbol "->"
        TArr t <$> typ

boolLit :: Parser Term
boolLit = B True  <$ keyword "true"
      <|> B False <$ keyword "false"

ite :: Parser Term
ite = do 
    keyword "if"
    c <- term
    keyword "then"
    t <- term
    keyword "else"
    Ite c t <$> term

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
    args <- choice [pure <$> binding, many1 $ parens binding]
    symbol "."
    body <- term
    pure $ foldr (uncurry $ Lambda Map.empty) body args
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
    subterm = choice [parens term, boolLit, lambda, ite, fix, var]

parser :: Parser Term
parser = do
    spaces
    term <* eof