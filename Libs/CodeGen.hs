module CodeGen where

import Ast
import Symbol
import CGTest
import Register

import Data.Map as Map hiding (foldl, foldr, map)


cProgram :: Ast -> [[String]]
cProgram (Program _ vds fds) = bind (cGlobalVarDef vds) (cFuncDefs fds)
    where bind (a, b) c = [a, c b]


cGlobalVarDef :: [Ast] -> ([String], RegTable)
cGlobalVarDef defs = foldr step ([], empty_rgt) defs where
    step (VarDef t xs) (l, rgt) =
        let
            rst = case t of
                TInt  -> foldr (step' 4) ([], empty_rgt) xs
                TChar -> foldr (step' 1) ([], empty_rgt) xs
        in
            (fst rst ++ l, Map.union (snd rst) rgt)

    step' s (Identifier i) (l, rgt) =
        (("\t.comm\t" ++ i ++ "," ++ show s) : l, Map.insert i (i ++ "(%rip)" , s) rgt)
    step' s (Array (Identifier i) (Number n)) (l, rgt) =
        (("\t.comm\t" ++ i ++ "," ++ show (n * s)) : l, Map.insert i (i ++ "(%rip)", s) rgt)


cFuncDefs :: [Ast] -> RegTable -> [String]
cFuncDefs fds rgt = concat $ foldr (\fd zero -> cAFuncDef fd rgt : zero) [] fds


cAFuncDef :: Ast -> RegTable -> [String]
cAFuncDef (FuncDef ft (Identifier fn) pl fb) rgt =
    [fn ++ ":", "\tpushq\t%rbp", "\tmovq\t%rsp, %rbp"]
        ++ bind (cParams pl) (cComdStmt fb)
        ++ ["\tpopq\t%rbp", "\tret"]
    where
        bind :: ([a], RegTable) -> (RegTable -> [a]) -> [a]
        bind (a, b) c = (++) a $ c $ Map.union b rgt

        cParams pls = (for_pl pls, fromList $ get_param_reg pls)

        get_param_reg pls =
            let l2 = unzip $ map (\(t, Identifier i) -> ((\t -> case t of TInt -> 4; TChar -> 1) t, i)) pls
                regs = map (\a -> show a ++ "(%rbp)") $ [-4,-8..(-24)] ++ [16,32..]
            in zip (snd l2) $ zip regs $ fst l2 -- (name, (reg, size))

        for_pl pls =
            let pl = get_param_reg pls
                si = unzip $ map (\(_,(_,c))->case c of 4->(1,"movl"); 1->(3,"movb")) pl
                reg = map (\inp->(registers!!inp)!!(fst si!!(inp-1))) [1..6]
            in map (\(a,b,c)->"\t" ++ a ++ "\t" ++ b ++ ", " ++ c) $ zip3 (snd si) reg $ map (\(_,(b,_))->b) pl


cComdStmt fb rgt = [""]


