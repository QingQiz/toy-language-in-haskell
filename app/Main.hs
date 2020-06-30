module Main where

import Ast
import CFG
import Parser
import Grammar
import CodeGen
import Semantic
import Functions
import Optimizer
import PeepHole

import System.Exit
import System.Environment


put = putStrLn . unlines

getAst' (Just a) = a
getAst' Nothing = error "Semantic error"

run is_o (fn:xs) = do
    s <- readFile fn
    case buildAst s of
        Right a -> if is_o
                   then put $ finalDash $ optimize $ runCodeGen $ getAst' $ runSema a
                   else put $ finalDash $ runCodeGen $ getAst' $ runSema a
        a -> putErr a

main :: IO ()
main = do
    args <- getArgs
    if "-O" `elem` args
        then run True $ filter (/="-O") args
        else run False args
