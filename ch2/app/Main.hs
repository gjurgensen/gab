module Main where

import System.Console.Haskeline

import Parse
import Eval

main :: IO ()
main = runInputT defaultSettings repl

repl :: InputT IO ()
repl = do 
    Just ln <- getInputLine "> "
    outputStrLn $ either id show $ interp ln "REPL"
    repl
