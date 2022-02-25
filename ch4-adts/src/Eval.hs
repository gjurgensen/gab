module Eval where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.Parsec

import Ast
import Parse
import Type

eval :: Env -> Term -> Maybe Term
eval env (Var v) = Map.lookup v env
eval env (Lambda _ var term) = pure $ Lambda env var term
eval env (App fun arg) = do 
    Lambda env' var term <- eval env fun
    arg <- eval env arg
    eval (Map.insert var arg env') term
eval env (Fix t) = do
    lam@(Lambda env' var body) <- eval env t
    eval (Map.insert var (Fix lam) env') body

mapLeft :: (t -> a) -> Either t b -> Either a b
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x

bimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
bimap f _ (Left x)  = Left $ f x
bimap _ g (Right x) = Right $ g x


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just x) = Right x
maybeToEither x Nothing  = Left x

interp :: String -> String -> Ctx -> Env -> Either String Term
interp inp src ctx env = do
    term <- mapLeft show $ parse (full term) src inp
    maybeToEither "Ill-typed" $ inferType ctx term
    maybeToEither "Evaluation error" $ eval env term

typeInterp :: String -> String -> Ctx -> Either String Type
typeInterp inp src ctx = do
    term <- mapLeft show $ parse (full term) src inp
    maybeToEither "Ill-typed" $ inferType ctx term

load :: String -> String -> Either String [(Ident, Term)]
-- load inp src = mapLeft show $ parse (full program) src inp
load inp src = bimap show (mapMaybe getBind) $ parse (full program) src inp
  where
    getBind (Bind i t) = Just (i,t)
    getBind _ = Nothing

loadFile :: String -> IO (Either String [(Ident, Term)])
loadFile path = do
    inp <- readFile path
    pure $ load inp path