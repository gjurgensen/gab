{-# LANGUAGE TypeOperators #-}
module Misc where

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

bimapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
bimapPair f g (a, b) = (f a, g b)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)
