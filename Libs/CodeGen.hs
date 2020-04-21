module CodeGen where

import Ast
import Symbol
import CGTest

import Data.Map as Map hiding (foldl, foldr, map)


cProgram :: Ast -> SymbolTable -> [[String]]
cProgram (Program _ vds fds) st = [cGlobalVarDef vds, cFuncDefs fds st]

cGlobalVarDef :: [Ast] -> [String]
cGlobalVarDef defs = foldr step [] defs where
    step (VarDef t xs) zero = (case t of
        TInt  -> foldr (step' 4) [] xs
        TChar -> foldr (step' 1) [] xs) ++ zero
    step' s (Identifier i) zero =
        ("\t.comm\t" ++ i ++ "," ++ show s) : zero
    step' s (Array (Identifier i) (Number n)) zero =
        ("\t.comm\t" ++ i ++ "," ++ show (n * s)) : zero


cFuncDefs :: [Ast] -> SymbolTable -> [String]
cFuncDefs fds st = flatten $ foldr (\fd zero -> cAFuncDef fd st : zero) [] fds
    where
        flatten = foldr (\x zero -> x ++ zero) []


cAFuncDef :: Ast -> st -> [String]
cAFuncDef (FuncDef _ (Identifier fn) _ _) st = [fn]




