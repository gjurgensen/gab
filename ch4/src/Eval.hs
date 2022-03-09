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
    toUTree (Left Lambda{}) = Node "Lambda" []
    toUTree (Left (App f x)) = Node "App"
        [toUTree (Left f :: Term + Pattern),
         toUTree (Left x :: Term + Pattern)]
    toUTree (Left (Fix f)) = Node "Fix" [toUTree (Left f :: Term + Pattern)]
    toUTree (Right p) = patToUTree p
    toUTree _ = undefined
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

unifyPat :: Pattern -> Term -> Maybe Env
unifyPat p t = toEnv <$> solveUnifier @String (unify (Right p) (Left t))
  where 
    toEnv :: Map.Map Int (Term + Pattern) -> Env
    toEnv m = mapCompose (getLeft <$> m) (numPatVars p)

eval :: Env -> Term -> Maybe Term
eval env (Constr c) = pure $ Constr c
eval env (Case t arms) = do
    scrutinee <- eval env t
    getFirst <$> foldMap (fmap First . tryArm scrutinee) arms
  where
    tryArm :: Term -> (Pattern, Term) -> Maybe Term
    tryArm t (p, t') = do
        env' <- unifyPat p t
        eval (env' `Map.union` env) t'
eval env (Var v) = Map.lookup v env
eval env (Lambda _ var term) = pure $ Lambda env var term
eval env (App fun arg) = do 
    fun <- eval env fun
    arg <- eval env arg
    case fun of 
        Lambda env' var term -> eval (Map.insert var arg env') term
        _ -> pure $ App fun arg
eval env (Fix t) = do
    lam@(Lambda env' var body) <- eval env t
    eval (Map.insert var (Fix lam) env') body

evalStmts :: Foldable f => Env -> f Stmt -> Env
evalStmts = foldr go
  where
    go (Bind i t) = Map.insert i t
    go _ = id

interp :: String -> String -> Ctx -> Env -> Either String Term
interp inp src ctx env = do
    term <- mapLeft show $ parse (full term) src inp
    maybeToEither "Ill-typed" $ inferType ctx term
    maybeToEither "Evaluation error" $ eval env term

typeInterp :: String -> String -> Ctx -> Either String Type
typeInterp inp src ctx = do
    term <- mapLeft show $ parse (full term) src inp
    maybeToEither "Ill-typed" $ inferType ctx term

load :: String -> String -> Either String [Stmt]
load inp src = mapLeft show $ parse (full program) src inp

loadFile :: String -> IO (Either String [Stmt])
loadFile path = do
    inp <- readFile path
    pure $ load inp path