{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Type where

import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Class
import Control.Monad.State.Lazy
import Data.Foldable

import Ast
import Unification
import Misc

instance Unifiable String Type where
    toUTree (TVar i)   = Node "TVar" [Node i []]
    toUTree (TArr x y) = Node "TArr" [toUTree x, toUTree y]
    toUTree (TUnif i)  = Leaf i
    fromUTree (Node "TVar" [Node i []]) = pure $ TVar i
    fromUTree (Node "TArr" [x, y]) = TArr <$> fromUTree x <*> fromUTree y
    fromUTree (Leaf i) = pure $ TUnif i
    fromUTree _ = Nothing

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

forgetful :: MonadState s m => m b -> m b
forgetful tc = do 
    c <- get
    a <- tc
    put c
    pure a

foreach :: (Traversable t, MonadState s m) => (a -> m b) -> t a -> m (t b)
foreach = traverse . (forgetful .)

uncurryArr :: Type -> ([Type], Type)
uncurryArr (TArr x y) = mapFst (x:) $ uncurryArr y
uncurryArr t = ([], t)

curryArr :: [Type] -> Type -> Type
curryArr codoms dom = foldr TArr dom codoms

inferType' :: Unifiable r Type => Term -> TypeCtx r Type
inferType' (Constr i) = lookupCtx i
inferType' (Case t arms) = do
    a <- inferType' t
    (tpats, tarms) <- unzip <$> foreach typeArm arms
    lift $ foldrM unify a tpats
    n <- lift freshUnifVar
    lift $ foldrM unify (TUnif n) tarms >>= canonical
  where
    typeArm (pat, arm) =
        (,) <$> typePat pat <*> inferType' arm
    typePat :: Unifiable r Type => Pattern -> TypeCtx r Type
    typePat (PVar i) = do
        n <- lift freshUnifVar
        insertCtx i $ TUnif n
        pure $ TUnif n
    typePat (PCon i ps) = do
        (codom, dom) <- uncurryArr <$> inferType' (Constr i)
        traverse_ bindArgs $ zip ps codom
        pure $ curryArr (drop (length ps) codom) dom
    bindArgs (p, t) = typePat p >>= lift . unify t
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
inferType' (Annot t typ) = do
    typ' <- inferType' t
    lift $ unify typ typ' >>= canonical


inferType :: Ctx -> Term -> Maybe Type
inferType c t = fmap normalizeType $ evalUnifier @String
              $ evalStateT (inferType' t) c

typeCon :: Ident -> [Type] -> Type
typeCon = foldr TArr . TVar

-- typeEnv :: Foldable f => Ctx -> f Stmt -> Maybe Ctx
-- typeEnv = foldrM go
--   where
--     go (Bind i term) ctx = do
--         typ <- inferType ctx term
--         pure $ Map.insert i typ ctx
--     go (Adt i cons) ctx = pure
--                         $ foldr (uncurry Map.insert) ctx
--                         $ fmap (typeCon i) <$> cons

typeEnv :: Foldable f => Ctx -> f Stmt -> Maybe Ctx
typeEnv = foldlM go
  where
    go ctx (Bind i term) = do
        typ <- inferType ctx term
        pure $ Map.insert i typ ctx
    go ctx (Adt i cons) = pure
                        $ foldr (uncurry Map.insert) ctx
                        $ fmap (typeCon i) <$> cons