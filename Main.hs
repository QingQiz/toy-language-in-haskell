module Main where

import Ast
import Symbol
import Grammar
import Semantic
import CodeGen

import System.Environment

get_ast :: Maybe (Ast, String) -> Ast
get_ast (Just (a, b)) = case b of
    "" -> a
    _  -> error $ "Parse Error On:\n" ++ b

get_ast' :: Maybe Ast -> Ast
get_ast' (Just a) = a
get_ast' Nothing = error $ "Semantic error"

run (fn:xs) = do
    s <- readFile fn
    let res = unlines $ cProgram $ get_ast' $ semaProgram $ get_ast $ build_ast s
    putStrLn $ res

main :: IO ()
main = do
    args <- getArgs
    run args

