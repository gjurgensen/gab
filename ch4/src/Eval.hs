{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Eval where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Foldable
import Data.Semigroup
import Control.Applicative
import Text.Parsec

import Misc
import Unification
import Ast
import Parse
import Type

-- Restricted to constrs and applications
instance Unifiable String (Term + Pattern) where
    toUTree (Left (Constr c)) = Node "Constr" [Node c []]
    toUTree (Left (Var v)) = Node "Var" [Node v []]
    toUTree (Left Lambda{}) = Node "Lambda" []
    toUTree (Left (App f x)) = Node "App"
        [toUTree (Left f :: Term + Pattern),
         toUTree (Left x :: Term + Pattern)]
    toUTree (Left (Fix f)) = Node "Fix" [toUTree (Left f :: Term + Pattern)]
    toUTree (Right p) = patToUTree p
    toUTree tp = error $ show tp
    fromUTree (Node "Constr" [Node c []]) = pure $ Left $ Constr c
    fromUTree (Node "App" [x,y]) = Left <$> liftA2 App
        (getLeft <$> fromUTree @_ @(Term + Pattern) x)
        (getLeft <$> fromUTree @_ @(Term + Pattern) y)
    fromUTree _ = Nothing

patToUTree :: Pattern -> UTree String
patToUTree = snd . go 0
  where
    go n (PVar _) = (n+1, Leaf n)
    go n (PCon i []) = (n, Node "Constr" [Node i []])
    go n (PCon i ps) = foldl app (n, Node "Constr" [Node i []]) ps
    app (n, t) p =
        let (n', t') = go n p
         in (n', Node "App" [t,t'])

numPatVars :: Pattern -> Map.Map Ident Int
numPatVars = snd . go (0, Map.empty)
  where
    go (n,m) (PVar i) = (n+1, Map.insert i n m)
    go (n,m) (PCon _ ps) = foldl go (n,m) ps

mapCompose :: (Ord a, Ord b) => Map.Map b c -> Map.Map a b -> Map.Map a c
mapCompose f g = foldr go Map.empty (Map.toList g)
  where
    go (k,v) m = case Map.lookup v f of
      Nothing -> m
      Just v' -> Map.insert k v' m

unifyPat :: Pattern -> Term -> UnifErr (Term + Pattern) + Env
unifyPat p t = toEnv <$> solveUnifier @String (unify (Right p) (Left t))
  where
    toEnv :: Map.Map Int (Term + Pattern) -> Env
    toEnv m = mapCompose (getLeft <$> m) (numPatVars p)

eval :: Env -> Term -> UnifErr Term + Term
eval env (Constr c) = pure $ Constr c
eval env (Case t arms) = do
    scrutinee <- eval env t
    (env', t') <- maybeToEither err $ getFirst <$> foldMap (matchArm scrutinee) arms
    eval (env' `Map.union` env) t'
  where
    matchArm :: Term -> (Pattern, Term) -> Maybe $ First (Env, Term)
    matchArm scrutinee (p, t) = do
        env' <- rightToMaybe $ unifyPat p scrutinee
        pure $ First (env', t)
    err = MiscErr "Failed to match on any pattern"
eval env (Var v) =
    maybeToEither err $ Map.lookup v env
  where
    err = MiscErr $ "Cannot evaluate unbound variable " ++ show v
eval env (Lambda _ var term) = pure $ Lambda env var term
eval env (App fun arg) = do
    fun <- eval env fun
    arg <- eval env arg
    case fun of
        Lambda env' var term -> eval (Map.insert var arg env') term
        _ -> pure $ App fun arg
eval env (Fix t) = do
    t <- eval env t
    case t of
        lam@(Lambda env' var body) -> eval (Map.insert var (Fix lam) env') body
        _ -> Left $ MiscErr "Cannot fix a non-function"
eval env (Annot t _) = eval env t

-- evalStmts :: Foldable f => Env -> f Stmt -> Env
-- evalStmts = foldr go
--   where
--     go (Bind i t) = Map.insert i t
--     go _ = id

evalStmts :: Foldable f => Env -> f Stmt -> UnifErr Term + Env
evalStmts = foldlM go
  where
    go env (Bind i t) = do
        t' <- eval env t
        pure $ Map.insert i t' env
    go env _ = pure env

interp :: String -> String -> Ctx -> Env -> String + Term
interp inp src ctx env = do
    term <- mapLeft show $ parse (full term) src inp
    mapLeft ((++) "Type error: " . show) $ inferType ctx term
    mapLeft ((++) "Evaluation error: " . show) $ eval env term

typeInterp :: String -> String -> Ctx -> String + Type
typeInterp inp src ctx = do
    term <- mapLeft show $ parse (full term) src inp
    mapLeft ((++) "Type error: " . show) $ inferType ctx term

load :: String -> String -> String + [Stmt]
load inp src = mapLeft show $ parse (full program) src inp

loadFile :: String -> IO (String + [Stmt])
loadFile path = do
    inp <- readFile path
    pure $ load inp path