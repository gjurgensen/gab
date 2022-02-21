module Main where

import System.Console.Haskeline
import Data.Char
import Data.List

import Parse
import Eval

main :: IO ()
main = runInputT defaultSettings repl

data Command = Exe | Type | Help | Quit

parseCommand :: String -> Maybe (Command, String)
parseCommand s = case stripPrefix ":" s of
    Just s -> case break isSpace s of
        ("t", s) -> pure (Type, s)
        ("h", s) -> pure (Help, s)
        ("q", s) -> pure (Quit, s)
        _ -> Nothing
    Nothing -> pure (Exe, s)

repl :: InputT IO ()
repl = do 
    Just ln <- getInputLine "> "
    case parseCommand ln of
        Just (c,s) -> command c s
        Nothing -> command Help ""
  where
    command Exe s = do 
        outputStrLn $ either id show $ interp s "REPL"
        repl
    command Type s = do
        outputStrLn $ either id show $ typeInterp s "REPL"
        repl
    command Help _ = do 
        outputStr $ unlines
            ["Commands:\n",
             "<expr> - evaluate <expr>",
             ":t <expr> - print type of <expr>",
             ":h - print commands",
             ":q - quit"]
        repl
    command Quit _ = pure ()