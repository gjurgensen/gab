module Eval where

import qualified Data.Map.Strict as Map
import Text.Parsec
import Ast
import Parse
import Type

eval :: Env -> Term -> Maybe Term
eval env val@(B _) = pure val
eval env (Var v) = Map.lookup v env
eval env (Lambda _ var term) = pure $ Lambda env var term
eval env (App fun arg) = do 
    Lambda env' var term <- eval env fun
    arg <- eval env arg
    eval (Map.insert var arg env') term
eval env (Ite c t e) = do
    B b <- eval env c
    eval env $ if b then t else e
eval env (Fix t) = do
    lam@(Lambda env' var body) <- eval env t
    eval (Map.insert var (Fix lam) env') body

mapLeft :: (t -> a) -> Either t b -> Either a b
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just x) = Right x
maybeToEither x Nothing  = Left x

interp :: String -> String -> Either String Term
interp inp src = do
    term <- mapLeft show $ parse parser src inp
    maybeToEither "Ill-typed" $ inferType term
    maybeToEither "Evaluation error" $ eval Map.empty term

typeInterp :: String -> String -> Either String Type
typeInterp inp src = do
    term <- mapLeft show $ parse parser src inp
    maybeToEither "Ill-typed" $ inferType term
