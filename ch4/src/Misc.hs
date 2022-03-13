{-# LANGUAGE TypeOperators #-}

module Misc where

import Control.Applicative

type f $ a = f a
infixr 2 $

type a + b = Either a b
infixl 4 +

getLeft :: a + b -> a
getLeft (Left x) = x
getLeft _ = undefined

mapLeft :: (t -> a) -> t + b -> a + b
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x

bimapSum :: (a -> c) -> (b -> d) -> a + b -> c + d
bimapSum f _ (Left x)  = Left $ f x
bimapSum _ g (Right x) = Right $ g x

maybeToEither :: a -> Maybe b -> a + b
maybeToEither _ (Just x) = Right x
maybeToEither x Nothing  = Left x

rightToMaybe :: a + b -> Maybe b
rightToMaybe Left{} = Nothing
rightToMaybe (Right x) = Just x

bimapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
bimapPair f g (a, b) = (f a, g b)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

applySum :: (Applicative f, Semigroup a) => f a -> f a -> f a
applySum = liftA2 (<>)

star1 :: Semigroup a => [a] -> [a]
star1 x = x ++ go x
  where
    go y = let sum = applySum x y in sum ++ go sum

star :: Monoid a => [a] -> [a]
star x = mempty ++ star1 x