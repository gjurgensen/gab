module Main where

import Eval

showInterp :: String -> IO ()
showInterp s = case interp s "test string" of
    Left  e -> putStrLn e
    Right t -> print t

main :: IO ()
main = do 
    showInterp "(λ x y. y) (λ x. x) (λ x. x)"
    showInterp "(λ x y. y) (λ x. x)"

