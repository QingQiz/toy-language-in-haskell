module Semantic where

import Ast
import Symbol
import Grammar

import Data.Char
import Data.Semigroup hiding ((<>))
import Data.Map as Map hiding (foldl, foldr, map)


semaProgram :: Ast -> Maybe Ast
semaProgram (Program cst var fun) =
    semaConst cst empty_st >>= semaVar var >>= semaFunc fun >>= rebuild where
        rebuild :: ([Ast], SymbolTable) -> Maybe Ast
        rebuild (asts, _) = Just $ Program [] var asts


semaConst :: [Ast] -> SymbolTable -> Maybe SymbolTable
semaConst [] st = Just st
semaConst (ConstDef t dfs:cs) st =
    semaConstDef t dfs st >>= semaConst cs where
        semaConstDef :: Type -> [(Ast, Ast)] -> SymbolTable -> Maybe SymbolTable
        semaConstDef TInt ((Identifier i, Number n):dfs) st =
            case Map.lookup i st of
                Nothing -> semaConstDef TInt dfs $ Map.insert i (SConst SInt n) st
                _       -> error $ "redefined const " ++ show i
        semaConstDef TChar ((Identifier i, Ch c):dfs) st =
            case Map.lookup i st of
                Nothing -> semaConstDef TChar dfs $ Map.insert i (SConst SChar (ord c)) st
                _       -> error $ "redefined const " ++ show i
        semaConstDef _ [] st = Just st
        -- wrong type , we can transform it to correct type
        semaConstDef _ _ _ = error $ "wrong type in const def"


semaVar :: [Ast] -> SymbolTable -> Maybe SymbolTable
semaVar [] st = Just st
semaVar (VarDef t dfs:vs) st = semaVarDef t dfs st >>= semaVar vs where
    semaVarDef :: Type -> [Ast] -> SymbolTable -> Maybe SymbolTable
    semaVarDef t (d:ds) st = case d of
        Identifier i -> case Map.lookup i st of
            Nothing -> semaVarDef t ds $ Map.insert i (pSVar t) st
            _       -> error $ "redefined var " ++ show i
        Array (Identifier i) (Number n) -> case Map.lookup i st of
            Nothing | n > 0 -> semaVarDef t ds $ Map.insert i (pSArr t n) st
                    | otherwise -> error $ "array size <= 0 in: " ++ show i
            _       -> error $ "redefined var " ++ show i
    semaVarDef _ [] st = Just st

    pSVar t = SVariable $ semaType t
    pSArr t = SArray $ semaType t


semaFunc :: [Ast] -> SymbolTable -> Maybe ([Ast], SymbolTable)
semaFunc [] st = Just ([], st)
semaFunc (f:fs) st = semaAFun f st >>> semaFunc fs where
    (>>>) :: Maybe (a, b) -> (b -> Maybe ([a], b)) -> Maybe ([a], b)
    step1 >>> stepo = case step1 of
        Just (x, st) -> case stepo st of
            Just (xs, st') -> Just (x:xs, st')
            _              -> Nothing
        _            -> Nothing

    semaAFun :: Ast -> SymbolTable -> Maybe (Ast, SymbolTable)
    semaAFun (FuncDef ft (Identifier fn) pl fb) st =
        case Map.lookup fn st of
            -- function params will replace global vars of the same name
            Nothing -> case semaParamList pl empty_st of
                Just nst -> let fs = pSFun ft pl
                                -- param list can cover function name
                                nst' = Map.union nst (Map.insert fn fs st)
                                -- insert ("", SFunction) to symbol table to ckeck ret_stmt
                            in case semaComdStmt fb (Map.insert "" fs nst') of
                                Just ast -> Just (FuncDef ft (Identifier fn) pl ast, Map.insert fn fs st)
                                _        -> Nothing
                Nothing  -> Nothing
            _       -> error $ "redefined function " ++ show fn

    semaParamList :: [(Type, Ast)] -> SymbolTable -> Maybe SymbolTable
    semaParamList ((t, Identifier i):pls) st = step (t, i) st >>= semaParamList pls where
        step (t, i) st = case Map.lookup i st of
            Nothing -> Just $ Map.insert i (SVariable $ semaType t) st
            _       -> error $ "param with a same name " ++ show i
    semaParamList [] st = Just st

    pSFun :: FunType -> [(Type, Ast)] -> Symbol
    pSFun t pl = SFunction (semaFType t) (map semaType $ foldr step [] pl)
        where step (a, b) zero = a : zero


semaComdStmt :: Ast -> SymbolTable -> Maybe Ast
semaComdStmt (ComdStmt cs vs sl) st =
    semaConst cs st >>= semaVar vs >>= semaStmtList sl >>= rebuild where
        rebuild :: Ast -> Maybe Ast
        rebuild ast = Just $ ComdStmt [] vs ast


semaStmtList :: Ast -> SymbolTable -> Maybe Ast
semaStmtList (StmtList stmts) st = toStmtList $ semaStmts stmts [] where
    toStmtList :: Maybe [Ast] -> Maybe Ast
    toStmtList (Just x) = Just $ StmtList x
    toStmtList Nothing  = Nothing

    semaStmts :: [Ast] -> [Ast] -> Maybe [Ast]
    semaStmts [] r = Just r
    semaStmts (stmt:stmts) zero = case semaAStmt stmt of
        Nothing -> Nothing
        x -> x <> semaStmts stmts zero

    semaAStmt :: Ast -> Maybe Ast
    semaAStmt (IfStmt c s es)   = semaIfStmt c s es
    semaAStmt sl@(StmtList _)   = semaStmtList sl st
    semaAStmt (ForStmt s e a b) = semaForStmt s e a b
    semaAStmt (DoStmt b c)      = semaDoStmt b c
    semaAStmt (Rd ids)          = semaReadStmt ids
    semaAStmt (Wt s e)          = semaWriteStmt s e
    semaAStmt (Ret e)           = semaRetStmt e
    semaAStmt (FuncCall n pl)   = semaFunCall n pl
    semaAStmt (Empty)           = Just Empty
    semaAStmt as@(Assign _ _)   = semaAssign as

    --             cond  stmt else-stmt
    semaIfStmt :: Ast -> Ast -> Ast -> Maybe Ast
    semaIfStmt c s es = toIfStmt $ case semaExpr c of
        Just (t, a) -> Just a <> semaAStmt s <> semaAStmt es <> Just []
        _           -> Nothing
        where
            toIfStmt Nothing = Nothing
            toIfStmt (Just [a, b, c]) = Just $ IfStmt a b c

    --             start   end   update  body
    semaForStmt :: Ast -> Ast -> Ast -> Ast -> Maybe Ast
    semaForStmt s e u b = toForStmt $ case semaExpr e of
        -- same to if stmt
        Just (t, a) -> semaAssign s <> Just a <> semaAssign u <> semaAStmt b <> Just []
        _           -> Nothing
        where
            toForStmt Nothing = Nothing
            toForStmt (Just [a, b, c, d]) = Just $ ForStmt a b c d

    --             stmt   cond
    semaDoStmt :: Ast -> Ast -> Maybe Ast
    semaDoStmt s c = toDoStmt $ case semaExpr c of
        Nothing -> Nothing
        Just (t, a) -> semaAStmt s <> Just a <> Just []
        where
            toDoStmt Nothing = Nothing
            toDoStmt (Just [a, b]) = Just $ DoStmt a b

    semaReadStmt :: [Ast] -> Maybe Ast
    semaReadStmt rd = toReadStmt $ for_rest rd where
        for_rest (Identifier x:xs) = check x <> for_rest xs
        for_rest [] = Just []

        check x = case Map.lookup x st of
            Just (SVariable _) -> Just $ Identifier x
            Nothing            -> error $ "read into a undefined variable " ++ show x
            _                  -> error $ "read to wrong type (maybe function?"

        toReadStmt Nothing = Nothing
        toReadStmt (Just a) = Just $ Rd a

    --                str    expr
    semaWriteStmt :: Ast -> Ast -> Maybe Ast
    semaWriteStmt s e = toWriteStmt $ case semaExpr e of
        Nothing -> Nothing
        Just (t, a) -> semaFormatStr s t <> Just a <> Just []
        where
            toWriteStmt Nothing = Nothing
            toWriteStmt (Just [a, b]) = Just $ Wt a b

    semaRetStmt :: Ast -> Maybe Ast
    semaRetStmt a = case semaExpr a of
        Nothing -> Nothing
        Just (t, x) -> case Map.lookup "" st of
            Just (SFunction SVoid _) -> case x of
                Empty -> Just $ Ret Empty
                _     -> error $ "return a none-void value in void function"
            -- maybe we should make a type conversion here
            Just (SFunction _ _) -> case x of
                Empty -> error $ "not return a value in a none-void function"
                x     -> Just $ Ret x
            Nothing -> error "???" -- this is not expected to happen

    --          func-name  param-list
    semaFunCall :: Ast -> [Ast] -> Maybe Ast
    semaFunCall (Identifier fn) pl = case Map.lookup fn st of
        Nothing -> error $ "call undefined function " ++ show fn
        Just (SFunction _ pld) -> toFuncCall $ foreach pld pl check
        where
            foreach (d:ds) (x:xs) f = f d x <> foreach ds xs f
            foreach [] [] _ = Just []
            foreach _  [] _ = error $ "param number error when call " ++ show fn
            foreach [] _  _ = error $ "param number error when call " ++ show fn

            check d x = case semaExpr x of
                Nothing -> Nothing
                -- maybe we can make a type conversion here
                Just (_, a) -> Just a

            toFuncCall Nothing = Nothing
            toFuncCall (Just x) = Just $ FuncCall (Identifier fn) x

    --           left   right
    semaAssign :: Ast -> Maybe Ast
    semaAssign (Assign l r) = case l of
        (Array _ _) -> case semaArray l of
            Nothing -> Nothing
            Just x  -> case semaExpr r of
                Nothing -> Nothing
                -- type conversion maybe
                Just (t, a) -> Just $ Assign x a
        (Identifier i) -> case Map.lookup i st of
            Just (SVariable _) -> case semaExpr r of
                Nothing -> Nothing
                -- type conversion maybe
                Just (t, a) -> Just $ Assign l a
            Nothing -> error $ "assign to a undefined variable " ++ show i
        _ -> error $ "assgin to wrong type"
    semaAssign Empty = Just Empty

    semaArray :: Ast -> Maybe Ast
    semaArray (Array (Identifier n) i) = case Map.lookup n st of
        Just (SArray _ _) -> case semaExpr i of
            Nothing -> Nothing
            Just (t, a) -> Just $ Array (Identifier n) a
        _ -> error $ "undefined or wrong type of " ++ show n

    semaFormatStr :: Ast -> ExprValue -> Maybe Ast
    -- check format string is boring, i don't want to do it...
    semaFormatStr a _ = Just a

    semaExpr x = semaExpr' x

    semaExpr' :: Ast -> Maybe (ExprValue, Ast)
    semaExpr' (BinNode o l r) = bind o (semaExpr' l) (semaExpr' r)
        where
            bind o a b = case flatten a b of
                Just (EStrictN c, _, EStrictN c', _) -> forOp o c c'
                Just (EArray, _, _, _) -> error $ show o ++ " is not allowed to a array value"
                Just (_, _, EArray, _) -> error $ show o ++ " is not allowed to a array value"
                Just (_, a, _, a') -> Just (EVariable, BinNode o a a')
                Nothing -> Nothing

            flatten (Just (v, a)) (Just (v', a')) = Just (v, a, v', a')
            flatten _ _ = Nothing

            forOp :: Op -> Int -> Int -> Maybe (ExprValue, Ast)
            forOp Mul a b = putValue $ a * b
            forOp Div a b = putValue $ a `div` b
            forOp Add a b = putValue $ a + b
            forOp Sub a b = putValue $ a - b
            forOp Gt  a b = putValue $ fromEnum (a >  b)
            forOp Ls  a b = putValue $ fromEnum (a <  b)
            forOp GE  a b = putValue $ fromEnum (a >= b)
            forOp LE  a b = putValue $ fromEnum (a <= b)
            forOp Equ a b = putValue $ fromEnum (a == b)
            forOp Neq a b = putValue $ fromEnum (a /= b)
            forOp And a b = putValue $ fromEnum (a /= 0 && b /= 0)
            forOp Or  a b = putValue $ fromEnum (a /= 0 || b /= 0)

            putValue v = Just (EStrictN v, Number v)

    semaExpr' (UnaryNode Not a) = case semaExpr' a of
        Just (EStrictN n, _) -> let x = fromEnum $ n == 0 in Just (EStrictN x, Number x)
        Just (ENot, (UnaryNode _ a)) -> Just (EVariable, a)
        Just (EArray, _) -> error $ "Not is not allowed to a array value"
        Just (_, a) -> Just (ENot, UnaryNode Not a)
        _ -> Nothing

    semaExpr' (Number n) = Just (EStrictN n, Number n)
    semaExpr' (Ch c) = Just(EStrictN $ ord c, Number $ ord c)

    semaExpr' a@(Array _ _) = case semaArray a of
        Nothing -> Nothing
        Just x -> Just (EVariable, a)

    semaExpr' (Identifier i) = case Map.lookup i st of
        Just (SVariable _) -> Just (EVariable, Identifier i)
        Just (SArray _ _) -> Just (EArray, Identifier i)
        Just (SConst _ v) -> Just (EStrictN v, Number v)
        _ -> error $ "unexpected identifier " ++ show i

    semaExpr' (FuncCall (Identifier fn) pl) = case Map.lookup fn st of
        Just (SFunction SVoid  _) -> error $ "call a void function " ++ show fn
        Just (SFunction _ _) -> case semaFunCall (Identifier fn) pl of
            Nothing -> Nothing
            Just a -> Just (EVariable, a)
        _ -> error $ "call a var or undefined function " ++ show fn

    semaExpr' Empty = Just (EVariable, Empty)

    -- instance (:) for Maybe
    infixr 5 <>
    (<>) :: Maybe a -> Maybe [a] -> Maybe [a]
    a <> b = case b of
        Nothing -> Nothing
        Just xs -> case a of
            Nothing -> Nothing
            Just  x -> Just (x:xs)


semaType :: Type -> SType
semaType TInt  = SInt
semaType TChar = SChar

semaFType :: FunType -> SType
semaFType FInt  = SInt
semaFType FChar = SChar
semaFType FVoid = SVoid

