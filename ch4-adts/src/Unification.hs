{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module Unification where

-- import Data.Tree
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Foldable
import Control.Monad.Trans.Class
import Control.Monad.State.Lazy
import Control.Monad.Trans.State.Lazy(StateT)

import Misc

data UTree a = Node a [UTree a] | Leaf Int
    deriving (Eq, Show)

instance Functor UTree where
    fmap f (Node x xs) = Node (f x) $ fmap f <$> xs
    fmap f (Leaf i) = Leaf i

instance Applicative UTree where
    pure x = Node x []
    Node f fs <*> tx@(Node x txs) = 
        Node (f x) $ fmap (f <$>) txs ++ fmap (<*> tx) fs
    Node{} <*> Leaf i = Leaf i
    Leaf i <*> _ = Leaf i

instance Monad UTree where
    return = pure
    Node x xs >>= f = case f x of 
        Node x' xs' -> Node x' $ xs' ++ fmap (>>= f) xs
        Leaf i -> Leaf i
    Leaf i >>= f = Leaf i


-- A partial ordering.
occursIn :: Eq a => UTree a -> UTree a -> Bool
x `occursIn` y@(Node _ ys) = x == y || any (x `occursIn`) ys
x `occursIn` y@Leaf{}      = x == y


-- Irreflexive/strict variant of occursIn
subsumedBy :: Eq a => UTree a -> UTree a -> Bool
x `subsumedBy` Node _ ys = any (x `occursIn`) ys
_ `subsumedBy` _ = False

infConstraint :: Eq a => UTree a -> UTree a -> Bool
infConstraint x y = x `subsumedBy` y || y `subsumedBy` x


type Solution a = Map.Map Int $ UTree a

underSol :: Solution a -> UTree a -> UTree a
underSol sol = go
  where
    go (Node x xs) = Node x $ go <$> xs
    go x@(Leaf i) = fromMaybe x $ Map.lookup i sol 

-- Adds unification binding, and also maps the new binding over the 
-- solution's codomain
bindUnif :: Int -> UTree a -> Solution a -> Solution a
bindUnif i t sol = Map.insert i t $ subst i t <$> sol
  where
    subst i = underSol . Map.singleton i

constrain :: Eq a => UTree a -> UTree a -> Solution a -> Maybe $ Solution a
constrain l r sol = 
    let l' = underSol sol l
        r' = underSol sol r
     in if l' == r' then
            pure sol
        else if infConstraint l' r' then
            Nothing
        else case (l', r') of
            (Leaf i, _) -> pure $ bindUnif i r' sol
            (_, Leaf i) -> pure $ bindUnif i l' sol
            (Node x xs, Node y ys) ->
                if x == y && length xs == length ys then
                    foldrM (uncurry constrain) sol $ zip xs ys
                else 
                    Nothing

class Eq r => Unifiable r a where
    toUTree :: a -> UTree r
    fromUTree :: UTree r -> Maybe a


data UnifierState r = UnifierState {sol :: Solution r, nextUnif :: Int}
data Unifier r x a = Unifiable r x =>
    Unifier {getUnifier :: StateT (UnifierState r) Maybe a}

instance Functor (Unifier r x) where
    fmap f (Unifier x) = Unifier $ fmap f x

instance Unifiable r x => Applicative (Unifier r x) where
    pure = Unifier . pure
    Unifier f <*> Unifier x = Unifier $ f <*> x

instance Unifiable r x => Monad (Unifier r x) where
    return = Unifier . return
    Unifier u >>= f = Unifier (u >>= getUnifier . f)

instance Unifiable r x => MonadState (UnifierState r) (Unifier r x) where
    state = Unifier . state

liftMaybe :: Unifiable r x => Maybe a -> Unifier r x a
liftMaybe = Unifier . lift

emptyState :: UnifierState r
emptyState = UnifierState Map.empty 0

unify :: Unifiable r x => x -> x -> Unifier r x x
unify t t' = do
    UnifierState s n <- get
    let (ut, ut') = (toUTree t, toUTree t')
    s' <- liftMaybe $ constrain ut ut' s
    put $ UnifierState s' n
    liftMaybe $ fromUTree $ underSol s' ut

freshUnifVar :: Unifiable r x => Unifier r x Int
freshUnifVar = do
    UnifierState s n <- get
    put $ UnifierState s (n+1)
    pure n

canonical :: Unifiable r x => x -> Unifier r x x
canonical x = do
    s <- sol <$> get
    liftMaybe $ fromUTree $ underSol s $ toUTree x

runUnifier :: Unifiable r x => Unifier r x a -> Maybe (a, Map.Map Int x)
runUnifier u = do
    (a, st) <- runStateT (getUnifier u) emptyState
    map <- traverse fromUTree $ sol st
    pure (a, map)

solveUnifier :: Unifiable r x => Unifier r x a -> Maybe $ Map.Map Int x
solveUnifier = fmap snd . runUnifier

evalUnifier :: Unifiable r x => Unifier r x a -> Maybe a
evalUnifier = fmap fst . runUnifier