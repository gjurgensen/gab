module Main where

import qualified Data.Map.Strict as Map
import System.Console.Haskeline
import Control.Monad.Trans.Class
import Data.Char
import Data.List

import Ast
import Type
import Parse
import Eval

main :: IO ()
main = runInputT defaultSettings $ repl Map.empty Map.empty

data Command = Eval | Type | Load | Help | Quit

parseCommand :: String -> Maybe (Command, String)
parseCommand s = case stripPrefix ":" s of
    Just s -> case break isSpace s of
        ("t", s) -> pure (Type, s)
        ("l", s) -> pure (Load, s)
        ("h", s) -> pure (Help, s)
        ("q", s) -> pure (Quit, s)
        _ -> Nothing
    Nothing -> pure (Eval, s)

repl :: Ctx -> Env -> InputT IO ()
repl ctx env = do 
    Just ln <- getInputLine "> "
    case parseCommand ln of
        Just (c,s) -> command c $ dropWhile isSpace s
        Nothing -> command Help ""
  where
    command Eval s = do 
        outputStrLn $ either id show $ interp s "REPL" ctx env
        repl ctx env
    command Type s = do
        outputStrLn $ either id show $ typeInterp s "REPL" ctx
        repl ctx env
    command Load s = do
        stmts <- lift $ loadFile s
        case stmts of 
            Left e -> outputStrLn e
            Right stmts -> case typeEnv ctx stmts of
                Nothing -> do
                    outputStrLn "Type error"
                    repl ctx env
                Just ctx' ->
                    repl ctx' (Map.fromList stmts `Map.union` env)
    command Help _ = do 
        outputStr $ unlines
            ["Commands:\n",
             "<expr> - evaluate <expr>",
             ":t <expr> - print type of <expr>",
             ":l <fileName> - load <fileName>",
             ":h - print commands",
             ":q - quit"]
        repl ctx env
    command Quit _ = pure ()