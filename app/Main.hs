module Main where

import Parser
import Grammar
import CodeGen
import Semantic
import Optimizer
import PeepHole

import System.Environment


put f = writeFile f . unlines

getAst' (Just a) = a
getAst' Nothing = error "Semantic error"

run is_o (fn:xs) = do
    s <- readFile fn
    case buildAst s of
        Right a -> if is_o
                   then put (fn ++ ".s") $ finalDash $ optimize $ runCodeGen $ getAst' $ runSema a
                   else put (fn ++ ".s") $ finalDash $ runCodeGen $ getAst' $ runSema a
        a -> putErr a

main :: IO ()
main = do
    args <- getArgs
    if "-O" `elem` args
        then run True $ filter (/="-O") args
        else run False args
