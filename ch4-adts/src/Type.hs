{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Type where

import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Class
import Control.Monad.State.Lazy

import Ast
import Unification

instance Unifiable String Type where
    toUTree (TVar i)   = Node "TVar" [Node i []]
    toUTree (TArr x y) = Node "TArr" [toUTree x, toUTree y]
    toUTree (TUnif i)  = Leaf i
    fromUTree (Node "TVar" [Node i []]) = TVar i
    fromUTree (Node "TArr" [x, y]) = TArr (fromUTree x) (fromUTree y)
    fromUTree (Leaf i) = TUnif i
    fromUTree _ = undefined

type Ctx = Map.Map Ident Type
type TypeCtx r = StateT Ctx (Unifier r Type)

lookupCtx :: Unifiable r Type => Ident -> TypeCtx r Type
lookupCtx i = do
    c <- get
    t <- lift $ liftMaybe $ Map.lookup i c
    lift $ canonical t

insertCtx :: Unifiable r Type => Ident -> Type -> TypeCtx r ()
insertCtx i t = do
    c <- get
    put $ Map.insert i t c

inferType' :: Unifiable r Type => Term -> TypeCtx r Type
inferType' (Constr i) = lookupCtx i
inferType' Case{} = undefined
inferType' (Var v) = lookupCtx v
inferType' (Lambda _ var body) = do
    n <- lift freshUnifVar
    insertCtx var $ TUnif n
    codom <- inferType' body
    dom <- lift $ canonical $ TUnif n
    pure $ TArr dom codom
inferType' (App l r) = do
    a <- inferType' l
    b <- inferType' r
    n <- lift freshUnifVar
    lift $ unify a (TArr b (TUnif n))
    lift $ canonical $ TUnif n
inferType' (Fix t) = do
    a <- inferType' t
    n <- lift freshUnifVar
    lift $ unify a (TArr (TUnif n) (TUnif n))
    lift $ canonical $ TUnif n


inferType :: Ctx -> Term -> Maybe Type
inferType c t = fmap normalizeType $ evalUnifier @String
              $ evalStateT (inferType' t) c

typeEnv :: Foldable f => Ctx -> f (Ident, Term) -> Maybe Ctx
typeEnv = foldr ((=<<) . go) . Just
  where
    go :: (Ident, Term) -> Ctx -> Maybe Ctx
    go (i, term) ctx = do
        typ <- inferType ctx term
        pure $ Map.insert i typ ctx
