module Semantic where

import Grammar
import TransAst
import ParserAst

import Data.Map as Map hiding (foldl, map)

-- TODO insert reserve name into empty symbol table
empty_st = fromList []

transProgram :: Ast -> Maybe Ast
transProgram (Program cst var fun) =
    transConst cst empty_st >>= transVar var >>= transFunc fun >>= rebuild where
        rebuild :: ([Ast], SymbolTable) -> Maybe Ast
        rebuild (asts, st) = Just $ Program cst var asts


transConst :: [Ast] -> SymbolTable -> Maybe SymbolTable
transConst [] st = Just st
transConst (ConstDef t dfs:cs) st =
    transConstDef t dfs st >>= transConst cs where
        transConstDef :: Type -> [(Ast, Ast)] -> SymbolTable -> Maybe SymbolTable
        transConstDef TInt ((Identifier i, Number _):dfs) st =
            case Map.lookup i st of
                Nothing -> transConstDef TInt dfs $ Map.insert i (SConst SInt) st
                _       -> Nothing -- redef
        transConstDef TChar ((Identifier i, Ch _):dfs) st =
            case Map.lookup i st of
                Nothing -> transConstDef TChar dfs $ Map.insert i (SConst SChar) st
                _       -> Nothing -- redef
        transConstDef _ [] st = Just st
        transConstDef _ _ _ = Nothing -- wrong type , we can transform it to correct type


transVar :: [Ast] -> SymbolTable -> Maybe SymbolTable
transVar [] st = Just st
transVar (VarDef t dfs:vs) st = transVarDef t dfs st >>= transVar vs where
    transVarDef :: Type -> [Ast] -> SymbolTable -> Maybe SymbolTable
    transVarDef t (d:ds) st = case d of
        Identifier i -> case Map.lookup i st of
            Nothing -> transVarDef t ds $ Map.insert i (pSVar t) st
            _       -> Nothing -- redef
        Array (Identifier i) (Number n) -> case Map.lookup i st of
            Nothing -> transVarDef t ds $ Map.insert i (pSArr t n) st
            _       -> Nothing -- redef 
    transVarDef _ [] st = Just st

    pSVar t = SVariable $ transType t
    pSArr t = SArray $ transType t


transFunc :: [Ast] -> SymbolTable -> Maybe ([Ast], SymbolTable)
transFunc [] st = Just ([], st)
transFunc (f:fs) st = transAFun f st >>> transFunc fs where
    (>>>) :: Maybe (a, b) -> (b -> Maybe ([a], b)) -> Maybe ([a], b)
    step1 >>> stepo = case step1 of
        Just (x, st) -> case stepo st of
            Just (xs, st') -> Just (x:xs, st')
            _              -> Nothing
        _            -> Nothing

    transAFun :: Ast -> SymbolTable -> Maybe (Ast, SymbolTable)
    transAFun (FuncDef ft (Identifier fn) pl fb) st =
        case Map.lookup fn st of
            -- function params will replace global vars of the same name
            Nothing -> case transParamList pl (Map.insert fn STempSymbol empty_st) of
                Just nst -> let fs = pSFun ft pl
                                nst' = Map.insert fn fs (Map.union nst st)
                            in case transComdStmt fb nst' of
                                Just ast -> Just (ast, Map.insert fn fs st)
                                _        -> Nothing
                Nothing  -> Nothing
            _       -> Nothing -- func redef

    transParamList :: [(Type, Ast)] -> SymbolTable -> Maybe SymbolTable
    transParamList ((t, Identifier i):pls) st = step (t, i) st >>= transParamList pls where
        step (t, i) st = case Map.lookup i st of
            Nothing -> Just $ Map.insert i (SVariable $ transType t) st
            _       -> Nothing
    transParamList [] st = Just st

    pSFun :: FunType -> [(Type, Ast)] -> Symbol
    pSFun t pl = SFunction (transFType t) (map transType $ foldl step [] pl)
        where step zero (a, b) = a : zero


transComdStmt :: Ast -> SymbolTable -> Maybe Ast
transComdStmt (ComdStmt cs vs sl) st =
    transConst cs st >>= transVar vs >>= transStmtList sl >>= rebuild where
        rebuild :: Ast -> Maybe Ast
        rebuild ast = Just $ ComdStmt cs vs ast


transStmtList :: Ast -> SymbolTable -> Maybe Ast
transStmtList =  undefined








transType :: Type -> SType
transType TInt  = SInt
transType TChar = SChar

transFType :: FunType -> SType
transFType FInt  = SInt
transFType FChar = SChar
transFType FVoid = SVoid



test = do
    s <- readFile "../test/t1_p.c0"
    return $ build_ast s

test_const = do
    a <- test
    let c = testf1 $ testf a
    return $ transConst c (fromList [])

test_var = do
    a <- test
    let c = testf2 $ testf a
    return $ transVar c (fromList [])

testf (Just a) = fst a
testf1 (Program s1 s2 s3) = s1
testf2 (Program s1 s2 s3) = s2
testf3 (Program s1 s2 s3) = s3


