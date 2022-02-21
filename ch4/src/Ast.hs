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

insertEnv :: Ident -> Term -> Env -> Env
insertEnv i t = Env . Map.insert i t . getEnv

unionEnv :: Env -> Env -> Env
unionEnv e1 e2 = Env $ Map.union (getEnv e1) (getEnv e2)

data Type
    = TBool
    | TArr Type Type
    | TUnif Int
    deriving Eq


normalizeType :: Type -> Type
normalizeType = fst . go (Map.empty, 0)
  where
    go x TBool = (TBool, x)
    go (m, i) (TUnif j) = case Map.lookup j m of
        Just k  -> (TUnif k, (m, i))
        Nothing -> (TUnif i, (Map.insert j i m, i+1))
    go x (t1 `TArr` t2) = 
        let (t1', y) = go x t1
            (t2', z) = go y t2
         in (t1' `TArr` t2', z)

showParens :: String -> String
showParens s = "(" ++ s ++ ")"

instance Show Type where
    show t = showNormalType $ normalizeType t

showNormalType :: Type -> String
showNormalType TBool = "Bool"
showNormalType (TArr t1@(TArr _ _) t2) = concat [showParens $ showNormalType t1, " -> ", showNormalType t2]
showNormalType (TArr t1 t2) = concat [showNormalType t1, " -> ", showNormalType t2]
showNormalType (TUnif x) = "?" ++ show x

data Term 
    = B Bool
    | Ite Term Term Term
    | Var Ident
    | Lambda Env Ident Term
    | App Term Term
    | Fix Term
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
showTerm (Fix t) = unwords ["fix", show t]
showTerm t@(Lambda env var body) = 
    if nullEnv env then
        showLambda var body
    else 
        showParens (showLambda var body) ++ " " ++ show env
  where
    showLambda var t@Lambda{} = let (args, body) = unnestLambda (Lambda emptyEnv var t) in
        "λ " ++ unwords args ++ ". " ++ show body
    showLambda var b =
        "λ " ++ var ++ ". " ++ show body
    unnestLambda :: Term -> ([Ident], Term)
    unnestLambda (Lambda _ var body) = 
        let (args, body') = unnestLambda body in (var:args, body')
    unnestLambda body = ([], body)
