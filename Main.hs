module Main where

import Ast
import Parser
import Symbol
import Grammar
import Semantic
import CodeGen

import System.Exit
import System.Environment


get_ast' (Just a) = a
get_ast' Nothing = error $ "Semantic error"

run (fn:xs) = do
    s <- readFile fn
    case buildAst s of
        Right a -> putStrLn $ unlines $ runCodeGen $ get_ast' $ runSema a
        a -> putErr a

main :: IO ()
main = do
    args <- getArgs
    run args

