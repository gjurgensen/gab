{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Type where

import qualified Data.Map.Strict as Map
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.State.Lazy
import Data.Foldable
import Data.Maybe
import Data.Monoid

import Ast
    ( Stmt(..), Pattern(..), Term(..), Type(..), Ident, normalizeType )
import Unification
import Misc

-- Need better equality of tforall via normalization (alpha equivalence)
instance Unifiable String Type where
    toUTree (TVar i)   = Node "TVar" [Node i []]
    toUTree (TArr x y) = Node "TArr" [toUTree x, toUTree y]
    toUTree (TForall i t) = Node "TForall" [Node i [], toUTree t]
    toUTree (TUnif i)  = Leaf i
    fromUTree (Node "TVar" [Node i []]) = pure $ TVar i
    fromUTree (Node "TArr" [x, y]) = TArr <$> fromUTree x <*> fromUTree y
    fromUTree (Node "TForall" [Node i [], x]) = TForall i <$> fromUTree x
    fromUTree (Leaf i) = pure $ TUnif i
    fromUTree _ = Nothing

type Ctx = Map.Map Ident Type
type TypeCtx r = StateT Ctx (Unifier r Type)

lookupCtx :: Unifiable r Type => Ident -> TypeCtx r Type
lookupCtx i = do
    c <- get
    t <- lift $ liftMaybe err $ Map.lookup i c
    lift $ canonical t
  where
    err = MiscErr $ unwords ["Variable", show i, "not in scope"]

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
    a <- inferType' l >>= inst
    b <- inferType' r >>= inst
    n <- lift freshUnifVar
    lift $ unify a (TArr b (TUnif n))
    lift $ canonical $ TUnif n
inferType' (Fix t) = do
    a <- inferType' t >>= inst
    n <- lift freshUnifVar
    lift $ unify a (TArr (TUnif n) (TUnif n))
    lift $ canonical $ TUnif n
inferType' (Annot t typ) = do
    typ' <- inferType' t
    lift $ unify typ typ' >>= canonical


inferType :: Ctx -> Term -> UnifErr Type + Type
inferType c t = fmap normalizeType $ evalUnifier @String
              $ evalStateT (inferType' t) c

typeCon :: Ident -> [Type] -> Type
typeCon = foldr TArr . TVar

genericNames :: [Ident]
genericNames = map ("'" ++) $ star1 $ pure <$> ['a'..'z']

poly :: Type -> Type
poly t =
    let (t', (m, _)) = runState (bindings t) (Map.empty, genericNames)
    in  foldr TForall t' $ snd <$> Map.toList m
  where
    bindings :: Type -> State (Map.Map Int Ident, [Ident]) Type
    bindings t@TVar{} = pure t
    bindings (TArr x y) = TArr <$> bindings x <*> bindings y
    bindings t@TForall{} = pure t
    bindings t@(TUnif i) = do
        (m, ns) <- get
        case Map.lookup i m of
            Nothing -> do
                let ident = head ns
                put (Map.insert i ident m, tail ns)
                pure $ TVar ident
            Just var -> pure $ TVar var

-- substitutes *free* variables
substType :: Map.Map Ident Type -> Type -> Type
substType m t@(TVar v) = fromMaybe t $ Map.lookup v m
substType m (TArr x y) = TArr (substType m x) (substType m y)
substType m (TForall i t) = TForall i (substType (Map.delete i m) t)
substType m t@TUnif{} = t

inst :: Unifiable r Type => Type -> TypeCtx r Type
inst (TForall i t) = do
    n <- lift freshUnifVar
    inst $ substType (Map.singleton i (TUnif n)) t
inst t = pure t

peelForalls :: Type -> ([Ident], Type)
peelForalls (TForall x t) = 
    let (xs, t') = peelForalls t
    in  (x:xs, t')
peelForalls t = ([], t)

typeEnv :: Foldable f => Ctx -> f Stmt -> UnifErr Type + Ctx
typeEnv = foldlM go
  where
    go ctx (Bind i term) = do
        typ <- inferType ctx term
        pure $ Map.insert i (poly typ) ctx
        -- pure $ Map.insert i typ ctx
    go ctx (Adt i cons) = pure
                        $ foldr (uncurry Map.insert) ctx
                        $ fmap (typeCon i) <$> cons