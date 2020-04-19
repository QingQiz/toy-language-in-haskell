module Semantic where

import Grammar
import TransAst
import ParserAst

import Data.Char
import Data.Semigroup hiding ((<>))
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
        transConstDef TInt ((Identifier i, Number n):dfs) st =
            case Map.lookup i st of
                Nothing -> transConstDef TInt dfs $ Map.insert i (SConst SInt n) st
                _       -> error $ "redefined const " ++ show i
        transConstDef TChar ((Identifier i, Ch c):dfs) st =
            case Map.lookup i st of
                Nothing -> transConstDef TChar dfs $ Map.insert i (SConst SChar (ord c)) st
                _       -> error $ "redefined const " ++ show i
        transConstDef _ [] st = Just st
        -- wrong type , we can transform it to correct type
        transConstDef _ _ _ = error $ "wrong type in const def"


transVar :: [Ast] -> SymbolTable -> Maybe SymbolTable
transVar [] st = Just st
transVar (VarDef t dfs:vs) st = transVarDef t dfs st >>= transVar vs where
    transVarDef :: Type -> [Ast] -> SymbolTable -> Maybe SymbolTable
    transVarDef t (d:ds) st = case d of
        Identifier i -> case Map.lookup i st of
            Nothing -> transVarDef t ds $ Map.insert i (pSVar t) st
            _       -> error $ "redefined var " ++ show i
        Array (Identifier i) (Number n) -> case Map.lookup i st of
            Nothing -> transVarDef t ds $ Map.insert i (pSArr t n) st
            _       -> error $ "redefined var " ++ show i
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
            Nothing -> case transParamList pl empty_st of
                Just nst -> let fs = pSFun ft pl
                                -- param list can cover function name
                                nst' = Map.union nst (Map.insert fn fs st)
                                -- insert ("", SFunction) to symbol table to ckeck ret_stmt
                            in case transComdStmt fb (Map.insert "" fs nst') of
                                Just ast -> Just (ast, Map.insert fn fs st)
                                _        -> Nothing
                Nothing  -> Nothing
            _       -> error $ "redefined function " ++ show fn

    transParamList :: [(Type, Ast)] -> SymbolTable -> Maybe SymbolTable
    transParamList ((t, Identifier i):pls) st = step (t, i) st >>= transParamList pls where
        step (t, i) st = case Map.lookup i st of
            Nothing -> Just $ Map.insert i (SVariable $ transType t) st
            _       -> error $ "param with a same name " ++ show i
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
transStmtList (StmtList stmts) st = toStmtList $ transStmts stmts [] where
    toStmtList :: Maybe [Ast] -> Maybe Ast
    toStmtList (Just x) = Just $ StmtList x
    toStmtList Nothing  = Nothing

    transStmts :: [Ast] -> [Ast] -> Maybe [Ast]
    transStmts [] r = Just r
    transStmts (stmt:stmts) zero = case transAStmt stmt of
        Nothing -> Nothing
        Just a  -> transStmts stmts $ a:zero

    transAStmt :: Ast -> Maybe Ast
    transAStmt (IfStmt c s es)   = transIfStmt c s es
    transAStmt sl@(StmtList _)   = transStmtList sl st
    transAStmt (ForStmt s e a b) = transForStmt s e a b
    transAStmt (DoStmt b c)      = transDoStmt b c
    transAStmt (Rd ids)          = transReadStmt ids
    transAStmt (Wt s e)          = transWriteStmt s e
    transAStmt (Ret e)           = transRetStmt e
    transAStmt (FuncCall n pl)   = transFunCall n pl
    transAStmt (Empty)           = Just Empty
    transAStmt as@(Assign _ _)   = transAssign as

    --             cond  stmt else-stmt
    transIfStmt :: Ast -> Ast -> Ast -> Maybe Ast
    transIfStmt c s es = toIfStmt $ case transExpr c of
        Just (t, a) -> Just a <> transAStmt s <> transAStmt es <> Just []
        _           -> Nothing
        where
            toIfStmt Nothing = Nothing
            toIfStmt (Just [a, b, c]) = Just $ IfStmt a b c

    --             start   end   update  body
    transForStmt :: Ast -> Ast -> Ast -> Ast -> Maybe Ast
    transForStmt s e u b = toForStmt $ case transExpr e of
        -- same to if stmt
        Just (t, a) -> transAssign s <> Just a <> transAssign u <> transAStmt b <> Just []
        _           -> Nothing
        where
            toForStmt Nothing = Nothing
            toForStmt (Just [a, b, c, d]) = Just $ ForStmt a b c d

    --             stmt   cond
    transDoStmt :: Ast -> Ast -> Maybe Ast
    transDoStmt s c = toDoStmt $ case transExpr c of
        Nothing -> Nothing
        Just (t, a) -> transAStmt s <> Just a <> Just []
        where
            toDoStmt Nothing = Nothing
            toDoStmt (Just [a, b]) = Just $ DoStmt a b

    transReadStmt :: [Ast] -> Maybe Ast
    transReadStmt rd = toReadStmt $ for_rest rd where
        for_rest (Identifier x:xs) = check x <> for_rest xs
        for_rest [] = Just []

        check x = case Map.lookup x st of
            Just (SVariable _) -> Just $ Identifier x
            Nothing            -> error $ "read into a undefined variable " ++ show x
            _                  -> error $ "read to wrong type (maybe function?"

        toReadStmt Nothing = Nothing
        toReadStmt (Just a) = Just $ Rd a

    --                str    expr
    transWriteStmt :: Ast -> Ast -> Maybe Ast
    transWriteStmt s e = toWriteStmt $ case transExpr e of
        Nothing -> Nothing
        Just (t, a) -> transFormatStr s t <> Just a <> Just []
        where
            toWriteStmt Nothing = Nothing
            toWriteStmt (Just [a, b]) = Just $ Wt a b

    transRetStmt :: Ast -> Maybe Ast
    transRetStmt a = case transExpr a of
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
    transFunCall :: Ast -> [Ast] -> Maybe Ast
    transFunCall (Identifier fn) pl = case Map.lookup fn st of
        Nothing -> error $ "call undefined function " ++ show fn
        Just (SFunction _ pld) -> toFuncCall $ foreach pld pl check
        where
            foreach (d:ds) (x:xs) f = f d x <> foreach ds xs f
            foreach [] [] _ = Just []
            foreach _  [] _ = error $ "param number error when call " ++ show fn
            foreach [] _  _ = error $ "param number error when call " ++ show fn

            check d x = case transExpr x of
                Nothing -> Nothing
                -- maybe we can make a type conversion here
                Just (_, a) -> Just a

            toFuncCall Nothing = Nothing
            toFuncCall (Just x) = Just $ FuncCall (Identifier fn) x

    --           left   right
    transAssign :: Ast -> Maybe Ast
    transAssign (Assign l r) = case l of
        (Array _ _) -> case transArray l of
            Nothing -> Nothing
            Just x  -> case transExpr r of
                Nothing -> Nothing
                -- type conversion maybe
                Just (t, a) -> Just $ Assign x a
        (Identifier i) -> case Map.lookup i st of
            Just (SVariable _) -> case transExpr r of
                Nothing -> Nothing
                -- type conversion maybe
                Just (t, a) -> Just $ Assign l a
            Nothing -> error $ "assign to a undefined variable " ++ show i
        _ -> error $ "assgin to wrong type"
    transAssign Empty = Just Empty

    transArray :: Ast -> Maybe Ast
    transArray (Array (Identifier n) i) = case Map.lookup n st of
        Just (SArray _ _) -> case transExpr i of
            Nothing -> Nothing
            Just (t, a) -> Just $ Array (Identifier n) a
        _ -> error $ "undefined or wrong type of " ++ show n

    transFormatStr :: Ast -> ExprValue -> Maybe Ast
    -- check format string is boring, i don't want to do it...
    transFormatStr a _ = Just a

    transExpr x = transExpr' x

    transExpr' :: Ast -> Maybe (ExprValue, Ast)
    transExpr' (BinNode o l r) = bind o (transExpr' l) (transExpr' r)
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

    transExpr' (UnaryNode Not a) = case transExpr' a of
        Just (EStrictN n, _) -> let x = fromEnum $ n == 0 in Just (EStrictN x, Number x)
        Just (ENot, (UnaryNode _ a)) -> Just (EVariable, a)
        Just (EArray, _) -> error $ "Not is not allowed to a array value"
        Just (_, a) -> Just (ENot, UnaryNode Not a)
        _ -> Nothing

    transExpr' (Number n) = Just (EStrictN n, Number n)
    transExpr' (Ch c) = Just(EStrictN $ ord c, Number $ ord c)

    transExpr' a@(Array _ _) = case transArray a of
        Nothing -> Nothing
        Just x -> Just (EVariable, a)

    transExpr' (Identifier i) = case Map.lookup i st of
        Just (SVariable _) -> Just (EVariable, Identifier i)
        Just (SArray _ _) -> Just (EArray, Identifier i)
        Just (SConst _ v) -> Just (EStrictN v, Number v)
        _ -> error $ "unexpected identifier " ++ show i

    transExpr' (FuncCall (Identifier fn) pl) = case Map.lookup fn st of
        Just (SFunction SVoid  _) -> error $ "call a void function" ++ show fn
        Just (SFunction _ _) -> case transFunCall (Identifier fn) pl of
            Nothing -> Nothing
            Just a -> Just (EVariable, a)
        _ -> error $ "call a var or undefined function" ++ show fn

    transExpr' Empty = Just (EVariable, Empty)

    -- instance (:) for Maybe
    infixr 5 <>
    (<>) :: Maybe a -> Maybe [a] -> Maybe [a]
    a <> b = case b of
        Nothing -> Nothing
        Just xs -> case a of
            Nothing -> Nothing
            Just  x -> Just (x:xs)


transType :: Type -> SType
transType TInt  = SInt
transType TChar = SChar

transFType :: FunType -> SType
transFType FInt  = SInt
transFType FChar = SChar
transFType FVoid = SVoid

