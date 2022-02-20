module Eval where

import qualified Data.Map.Strict as Map
import Text.Parsec
import Ast
import Parse

eval :: Env -> Term -> Maybe Term
eval env (Var v) = Map.lookup v env
eval env (Lambda _ var term) = pure $ Lambda env var term
eval env (App fun arg) = do 
    Lambda env' var term <- eval env fun
    arg <- eval env arg
    eval (Map.insert var arg env') term

interp :: String -> String -> Either String Term
interp inp src = do
    term <- mapLeft show $ parse parser src inp
    maybeToEither "" $ eval Map.empty term
  where
    mapLeft f (Left x)  = Left $ f x
    mapLeft _ (Right x) = Right x
    maybeToEither _ (Just x) = Right x
    maybeToEither x Nothing  = Left x