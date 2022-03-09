{-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE FlexibleContexts #-}

module Parse where

import qualified Data.Map.Strict as Map
-- import Data.List.NonEmpty
import Text.Parsec
import Text.Parsec.String
import Data.Maybe
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
keywords = ["fn", "fix", "data", "def", "case", "of"]

ident :: Parser Ident
ident = tok (try $ do
    c  <- lower
    cs <- many identChar
    let str = c:cs
    if str `elem` keywords then
        unexpected "keyword"
    else pure str
    ) <?> "identifier"

upperIdent :: Parser Ident
upperIdent = tok (try $ do
    c  <- upper
    cs <- many identChar
    pure $ c:cs
    ) <?> "uppercase identifier"

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

constr :: Parser Term
constr = Constr <$> upperIdent

binding :: Parser (Ident, Type)
binding = do
    i <- ident
    symbol ":"
    t <- typ
    pure (i,t)

lambda :: Parser Term
lambda = tok ( do
    symbol "λ" <|> keyword "fn"
    args <- many1 ident
    symbol "."
    body <- term
    pure $ foldr (Lambda Map.empty) body args
    ) <?> "lambda expression"

fix :: Parser Term
fix = do
    keyword "fix"
    Fix <$> term

pat :: Parser Pattern
pat = parens pat <|> varPat <|> constrPat
  where
    varPat = PVar <$> ident
    constrPat = PCon <$> upperIdent <*> many pat

caseTerm :: Parser Term
caseTerm = do
    keyword "case"
    scrutinee <- term
    keyword "of"
    Case scrutinee <$> sepBy arm (symbol "|")
  where
    arm = do
        p <- pat
        symbol "=>"
        (p ,) <$> term

term :: Parser Term
term = do
    hd <- subterm
    tl <- many subterm
    pure $ foldl App hd tl
    <?> "term"
  where
    subterm = do
        t <- choice [parens term, lambda, fix, caseTerm, constr, var]
        option t $ do
            symbol ":"
            Annot t <$> typ

bindStmt :: Parser Stmt
bindStmt = do
    keyword "def"
    i <- ident
    args <- many ident
    symbol ":="
    t <- term
    pure $ Bind i $ foldr (Lambda Map.empty) t args

comment :: Parser ()
comment = tok $ do
    symbol "//"
    many $ noneOf "\n"
    pure ()

adt :: Parser Stmt
adt = do
    keyword "data"
    name <- ident
    symbol ":="
    cons <- sepBy constructor $ symbol "|"
    pure $ Adt name cons
    <?> "ADT"
  where
    constructor = (,) <$> upperIdent <*> many typ

statement :: Parser Stmt
statement = bindStmt <|> adt <?> "statement"

foo :: Parser [Maybe Stmt]
foo = many $ (Nothing <$ comment) <|> (pure <$> statement <* symbol ";")

program :: Parser [Stmt]
program =
   catMaybes <$> many ((Nothing <$ comment) <|> (pure <$> statement <* symbol ";"))

full :: Parser a -> Parser a
full p = spaces >> (p <* eof)