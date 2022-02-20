module Ast where

import qualified Data.Map.Strict as Map

type Ident = String

type Env = Map.Map Ident Term

data Term 
    = Var Ident
    | Lambda Env Ident Term
    | App Term Term
    deriving (Eq, Show)