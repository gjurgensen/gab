module Type where

import qualified Data.Map.Strict as Map

import Ast

type Ctx = Map.Map Ident Type

typeCheck :: Ctx -> Term -> Maybe Type
typeCheck ctx (B _) = pure TBool
typeCheck ctx (Ite c t e) = do 
    TBool <- typeCheck ctx c
    a <- typeCheck ctx t
    b <- typeCheck ctx e
    if a == b then pure a else Nothing
typeCheck ctx (Var v) = Map.lookup v ctx
typeCheck ctx (Lambda _ var dom body) =
    TArr dom <$> typeCheck (Map.insert var dom ctx) body
typeCheck ctx (App l r) = do
    TArr dom codom <- typeCheck ctx l
    tr <- typeCheck ctx r
    if tr == dom then pure codom else Nothing
typeCheck ctx (Fix t) = do
    TArr dom codom <- typeCheck ctx t
    if dom == codom then pure dom else Nothing