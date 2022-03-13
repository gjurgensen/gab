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
import Misc

main :: IO ()
main = runInputT defaultSettings $ repl Map.empty Map.empty

data Command = Eval | Type | Stmt | Load | Help | Quit

parseCommand :: String -> Maybe (Command, String)
parseCommand s = case stripPrefix ":" s of
    Just s -> case break isSpace s of
        ("t", s) -> pure (Type, s)
        ("s", s) -> pure (Stmt, s)
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
    command Stmt s =
        let res = do
                stmts <- load s "REPL"
                ctx' <- mapLeft ((++) "Type error: " . show)
                        $ typeEnv ctx stmts
                env' <- mapLeft ((++) "Evaluation error: " . show)
                        $ evalStmts env stmts
                pure (ctx', env')
        in  case res of 
                Left e -> do
                    outputStrLn e
                    repl ctx env
                Right x -> uncurry repl x
    command Load s = do
        stmts <- lift $ loadFile s
        let res = do
                stmts <- stmts
                ctx' <- mapLeft ((++) "Type error: " . show)
                        $ typeEnv ctx stmts
                env' <- mapLeft ((++) "Evaluation error: " . show)
                        $ evalStmts env stmts
                pure (ctx', env')
        case res of 
            Left e -> do
                outputStrLn e
                repl ctx env
            Right x -> uncurry repl x
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