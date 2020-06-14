module Main where

import Ast
import CFG
import Parser
import Grammar
import CodeGen
import Semantic
import Functions
import Optimizer

import System.Exit
import System.Environment


put = putStrLn . unlines

get_ast' (Just a) = a
get_ast' Nothing = error $ "Semantic error"

run is_o (fn:xs) = do
    s <- readFile fn
    case buildAst s of
        Right a -> if is_o
                   then put $ optimize $ runCodeGen $ get_ast' $ runSema a
                   else put $ runCodeGen $ get_ast' $ runSema a
        a -> putErr a

main :: IO ()
main = do
    args <- getArgs
    if "-O" `elem` args
        then run True $ filter (\x -> x /= "-O") args
        else run False args
