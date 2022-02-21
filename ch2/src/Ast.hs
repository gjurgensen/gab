module Ast where

import qualified Data.Map.Strict as Map

type Ident = String

newtype Env = Env {getEnv :: Map.Map Ident Term}
    deriving Eq

instance Show Env where
    show (Env map) = show $ Map.toList map

emptyEnv :: Env
emptyEnv = Env Map.empty

nullEnv :: Env -> Bool
nullEnv = Map.null . getEnv

data Type
    = TBool
    | TArr Type Type
    deriving Eq

showParens :: String -> String
showParens s = "(" ++ s ++ ")"

instance Show Type where
    show TBool = "Bool"
    show (TArr t1@(TArr _ _) t2) = concat [showParens $ show t1, " -> ", show t2]
    show (TArr t1 t2) = concat [show t1, " -> ", show t2]

data Term 
    = B Bool
    | Ite Term Term Term
    | Var Ident
    | Lambda Env Ident Type Term
    | App Term Term
    deriving Eq

instance Show Term where
    show t = showTerm t

showTerm :: Term -> String
showTerm (B True)  = "true"
showTerm (B False) = "false"
showTerm (Ite c t e) = unwords ["if", show c, "then", show t, "else", show e]
showTerm (Var i) = i
showTerm (App t1 t2@(App _ _)) = unwords [show t1, showParens $ show t2]
showTerm (App t1 t2) = unwords [show t1, show t2]
showTerm t@(Lambda env var typ body) = 
    if nullEnv env then
        showLambda var typ body
    else 
        showParens (showLambda var typ body) ++ " " ++ show env
  where
    showLambda var env t@Lambda{} = let (args, body) = unnestLambda (Lambda emptyEnv var env t) in
        "λ " ++ unwords (showParens . uncurry showBinding <$> args) ++ ". " ++ show body
    showLambda var typ b =
        "λ " ++ showBinding var typ ++ ". " ++ show body
    showBinding var typ = var ++ ": " ++ show typ
    unnestLambda :: Term -> ([(Ident, Type)], Term)
    unnestLambda (Lambda _ var typ body) = 
        let (args, body') = unnestLambda body in ((var, typ):args, body')
    unnestLambda body = ([], body)
