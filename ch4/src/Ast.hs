module Ast where

import qualified Data.Map.Strict as Map
import Data.List

type Ident = String

type Env = Map.Map Ident Term

showEnv :: Env -> String
showEnv = show . Map.toList

data Type
    = TVar Ident
    | TArr Type Type
    | TForall Ident Type
    | TUnif Int
    deriving Eq


normalizeType :: Type -> Type
normalizeType = fst . go (Map.empty, 0)
  where
    go x t@(TVar _) = (t, x)
    go x@(m, i) (TUnif j) = case Map.lookup j m of
        Just k  -> (TUnif k, x)
        Nothing -> (TUnif i, (Map.insert j i m, i+1))
    go x (TForall i t) = 
        let (t', y) = go x t
        in  (TForall i t', y)
    go x (t1 `TArr` t2) = 
        let (t1', y) = go x t1
            (t2', z) = go y t2
         in (t1' `TArr` t2', z)

showParens :: String -> String
showParens s = "(" ++ s ++ ")"

instance Show Type where
    show t = showNormalType $ normalizeType t

showNormalType :: Type -> String
showNormalType (TVar i) = i
showNormalType (TArr t1@(TArr _ _) t2) = unwords [showParens $ showNormalType t1, "->", showNormalType t2]
showNormalType (TArr t1 t2) = unwords [showNormalType t1, "->", showNormalType t2]
showNormalType (TUnif x) = "?" ++ show x
showNormalType (TForall i t) = showForall i t
  where
    showForall i t@TForall{} = 
        let (is, t') = unnestForalls t
        in  unwords ["∀", unwords (i:is) ++ ".", showNormalType t']
    showForall i t = 
        unwords ["∀", i ++ ".", showNormalType t]
    unnestForalls (TForall i t) = 
        let (is, t') = unnestForalls t
        in  (i:is, t')
    unnestForalls t = ([], t)


data Term 
    = Constr Ident
    | Case Term [(Pattern, Term)]
    | Var Ident
    | Lambda Env Ident Term
    | App Term Term
    | Fix Term
    | Annot Term Type
    deriving Eq

data Pattern 
    = PVar Ident
    | PCon Ident [Pattern]
    deriving Eq

instance Show Pattern where
    show (PVar i) = i
    show (PCon c ps) = unwords $ c : fmap showPatInner ps


showPatInner :: Pattern -> String
showPatInner x@PCon{} = showParens $ show x
showPatInner x = show x

instance Show Term where
    show t = showTerm t

showTerm :: Term -> String
showTerm (Constr i) = i
showTerm (Case t arms) = unwords $ ["case", show t, "of"] ++
    intersperse "|" (showArm <$> arms)
  where
    showArm (p, t) = unwords [show p, "=>", show t]
showTerm (Var i) = i
showTerm (App t1 t2@(App _ _)) = unwords [show t1, showParens $ show t2]
showTerm (App t1 t2) = unwords [show t1, show t2]
showTerm (Fix t) = unwords ["fix", show t]
showTerm (Annot term typ) = unwords [show term, ":", show typ]
showTerm t@(Lambda env var body) = 
    if Map.null env then
        showLambda var body
    else 
        showParens (showLambda var body) ++ " " ++ showEnv env
  where
    showLambda var t@Lambda{} = let (args, body) = unnestLambda (Lambda Map.empty var t) in
        "λ " ++ unwords args ++ ". " ++ show body
    showLambda var b =
        "λ " ++ var ++ ". " ++ show body
    unnestLambda :: Term -> ([Ident], Term)
    unnestLambda (Lambda _ var body) = 
        let (args, body') = unnestLambda body in (var:args, body')
    unnestLambda body = ([], body)

data Stmt
    = Bind Ident Term
    | Adt Ident [(Ident, [Type])]
