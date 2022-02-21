module Eval where

import qualified Data.Map.Strict as Map
import Text.Parsec
import Ast
import Parse
import Type

eval :: Env -> Term -> Maybe Term
eval env val@(B _) = pure val
eval env (Var v) = Map.lookup v $ getEnv env
eval env (Lambda _ var t term) = pure $ Lambda env var t term
eval env (App fun arg) = do 
    Lambda env' var _ term <- eval env fun
    arg <- eval env arg
    eval (insertEnv var arg env') term
eval env (Ite c t e) = do
    B b <- eval env c
    eval env $ if b then t else e
eval env (Fix t) = do
    lam@(Lambda env' var typ body) <- eval env t
    eval (insertEnv var (Fix lam) env') body

mapLeft :: (t -> a) -> Either t b -> Either a b
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just x) = Right x
maybeToEither x Nothing  = Left x

interp :: String -> String -> Either String Term
interp inp src = do
    term <- mapLeft show $ parse parser src inp
    maybeToEither "Ill-typed" $ typeCheck Map.empty term
    maybeToEither "Evaluation error" $ eval emptyEnv term

typeInterp :: String -> String -> Either String Type
typeInterp inp src = do
    term <- mapLeft show $ parse parser src inp
    maybeToEither "Ill-typed" $ typeCheck Map.empty term
