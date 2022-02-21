-- {-# LANGUAGE LambdaCase #-}

module Type where

import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Maybe

import Ast


type ConstrSolution = Map.Map Int Type

-- A partial ordering.
occursIn :: Type -> Type -> Bool
x `occursIn` (TArr y z) = x `occursIn` y || x `occursIn` z
x `occursIn` y = x == y

subsumedBy :: Type -> Type -> Bool
-- Irreflexive/strict variant of occursIn
x `subsumedBy` (TArr y z) = x `occursIn` y || x `occursIn` z
_ `subsumedBy` _ = False

infConstraint :: Type -> Type -> Bool
infConstraint x y = x `subsumedBy` y || y `subsumedBy` x

underSol :: ConstrSolution -> Type -> Type
underSol sol = go 
  where
    go (TArr t1 t2) = TArr (go t1) (go t2)
    go t@(TUnif i) = fromMaybe t $ Map.lookup i sol
    go TBool = TBool

-- Adds unification binding, and also maps the new binding over the 
-- solution's codomain
bindUnif :: Int -> Type -> ConstrSolution -> ConstrSolution
bindUnif i t sol = Map.insert i t $ subst i t <$> sol
  where
    subst i = underSol . Map.singleton i

(===) :: Type -> Type -> ConstrSolution -> Maybe ConstrSolution
(l === r) sol = 
    let l' = underSol sol l
        r' = underSol sol r
     in if l' == r' then
            pure sol
        else if infConstraint l' r' then
            Nothing
        else case (l', r') of
            (TUnif i, _) -> pure $ bindUnif i r' sol
            (_, TUnif i) -> pure $ bindUnif i l' sol
            (TArr a b, TArr x y) -> (a === x) sol >>= (b === y)
            _ -> Nothing


type Ctx = Map.Map Ident Type
data TypeState = TypeState {ctx :: Ctx, sol :: ConstrSolution, nextUnif :: Int}
type UnifCtx = StateT TypeState Maybe

emptyTypeState :: TypeState
emptyTypeState = TypeState{ctx=Map.empty, sol= Map.empty, nextUnif=0}

unify :: Type -> Type -> UnifCtx Type
unify t t' = do
    st <- get
    sol' <- lift $ t === t' $ sol st
    put $ TypeState {ctx = ctx st, sol = sol', nextUnif = nextUnif st}
    pure $ underSol sol' t

freshUnifVar :: UnifCtx Int
freshUnifVar = do
    TypeState {ctx=c, sol=s, nextUnif=n} <- get
    put $ TypeState {ctx=c, sol=s, nextUnif=n+1}
    pure n

canonical :: Type -> UnifCtx Type
canonical t = do
    s <- sol <$> get
    pure $ underSol s t

lookupCtx :: Ident -> UnifCtx Type
lookupCtx i = do
    c <- ctx <$> get
    s <- sol <$> get
    t <- lift $ Map.lookup i c
    pure $ underSol s t

insertCtx :: Ident -> Type -> UnifCtx ()
insertCtx i t = do
    TypeState {ctx=c, sol=s, nextUnif=n} <- get
    put $ TypeState {ctx= Map.insert i t c, sol=s, nextUnif=n}

inferType' :: Term -> UnifCtx Type
inferType' (B _) = pure TBool
inferType' (Ite c t e) = do
    TBool <- inferType' c
    a <- inferType' t
    b <- inferType' e
    unify a b
inferType' (Var v) = lookupCtx v
inferType' (Lambda _ var body) = do
    n <- freshUnifVar
    insertCtx var $ TUnif n
    codom <- inferType' body
    dom <- canonical $ TUnif n
    pure $ TArr dom codom
inferType' (App l r) = do
    a <- inferType' l
    b <- inferType' r
    n <- freshUnifVar
    unify a (TArr b (TUnif n))
    canonical $ TUnif n
inferType' (Fix t) = do
    a <- inferType' t
    n <- freshUnifVar
    unify a (TArr (TUnif n) (TUnif n))
    canonical $ TUnif n

inferType :: Term -> Maybe Type
inferType t = normalizeType <$> evalStateT (inferType' t) emptyTypeState