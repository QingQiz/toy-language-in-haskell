module Main where

import Ast
import Symbol
import Grammar
import Semantic

import System.Environment

dispP :: Maybe (Ast, String) -> String
dispP (Just (a, b)) = case b of
    "" -> show a
    _  -> error $ "Parse Error On:\n" ++ b

dispP Nothing = "Nothing"

dispS :: Maybe (Ast, SymbolTable) -> String
dispS Nothing = "Nothing"
dispS (Just (a,b)) = show a

run (fn:xs) = do
    s <- readFile fn
    putStrLn $ dispP (build_ast s)
    putStrLn $ case build_ast s of
        Nothing -> "Nothing"
        Just (a, b) -> dispS $ semaProgram a

main :: IO ()
main = do
    args <- getArgs
    run args

