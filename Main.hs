module Main where

import Grammar
import System.Environment

disp :: Maybe (Ast, String) -> String
disp (Just (a, b)) = case b of
    "" -> "Parse Success, AST:\n" ++ show a
    _  -> "Parse Error On:\n" ++ b
disp Nothing = "Nothing"

run ('-':'-':'a':'s':'t':' ':xs) = do
    s <- readFile xs
    putStrLn $ disp (build_ast s)

run f@(x:xs) = putStrLn "Not Available"
run _        = putStrLn "Not Available" 

main :: IO ()
main = do
    args <- getArgs
    run $ unwords args

