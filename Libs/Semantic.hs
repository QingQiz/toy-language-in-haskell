module Semantic where

import Ast
import Symbol
import Grammar

import Data.Char
import Data.Map as Map hiding (foldl, foldr, map, splitAt)


runSema = semaProgram

semaProgram :: Ast -> Maybe Ast
semaProgram (Program sc cst var fun) =
    semaConst cst sc empty_st >>= semaVar var sc >>= semaFunc fun sc >>= rebuild where
        rebuild :: ([Ast], SymbolTable) -> Maybe Ast
        rebuild (asts, _) = Just $ Program sc [] var asts


semaConst :: [Ast] -> String -> SymbolTable -> Maybe SymbolTable
semaConst [] _ st = Just st
semaConst (ConstDef t _ dfs:cs) sc st =
    semaConstDef t dfs st >>= semaConst cs sc where
        semaConstDef :: Type -> [(Ast, Ast)] -> SymbolTable -> Maybe SymbolTable
        -- for int
        semaConstDef TInt ((Identifier p i, Number _ n):dfs) st =
            case Map.lookup i st of
                Nothing -> semaConstDef TInt dfs $ Map.insert i (SConst SInt n) st
                _       -> putSemaError sc p "Confilcting definations for const" i
        -- for char -> convert to int
        semaConstDef TChar ((Identifier p i, Ch _ c):dfs) st =
            case Map.lookup i st of
                Nothing -> semaConstDef TChar dfs $ Map.insert i (SConst SChar (ord c)) st
                _       -> putSemaError sc p "Confilcting definations for const" i
        semaConstDef _ [] st = Just st


semaVar :: [Ast] -> String -> SymbolTable -> Maybe SymbolTable
semaVar [] _ st = Just st
semaVar (VarDef t _ dfs:vs) sc st = semaVarDef t dfs st >>= semaVar vs sc where
    semaVarDef :: Type -> [Ast] -> SymbolTable -> Maybe SymbolTable
    semaVarDef t (d:ds) st = case d of
        Identifier p i -> case Map.lookup i st of
            Nothing -> semaVarDef t ds $ Map.insert i (pSVar t) st
            _       -> putSemaError sc p "Confilcting definations for variable" i
        Array _ (Identifier p1 i) (Number p2 n) -> case Map.lookup i st of
            Nothing | n > 0 -> semaVarDef t ds $ Map.insert i (pSArr t n) st
                    | otherwise -> putSemaError sc p2 "Array size must greater than" "0"
            _       -> putSemaError sc p1 "Confilcting definations for array" i
    semaVarDef _ [] st = Just st

    pSVar t = SVariable $ semaType t
    pSArr t = SArray $ semaType t


semaFunc :: [Ast] -> String -> SymbolTable -> Maybe ([Ast], SymbolTable)
semaFunc [] _ st = Just ([], st)
semaFunc (f:fs) sc st = semaAFun f st >>> semaFunc fs sc where
    (>>>) :: Maybe (a, b) -> (b -> Maybe ([a], b)) -> Maybe ([a], b)
    step1 >>> stepo = case step1 of
        Just (x, st) -> case stepo st of
            Just (xs, st') -> Just (x:xs, st')
            _              -> Nothing
        _ -> Nothing

    semaAFun :: Ast -> SymbolTable -> Maybe (Ast, SymbolTable)
    semaAFun (FuncDef ft pf (Identifier p fn) pl fb) st =
        case Map.lookup fn st of
            -- function params will replace global vars of the same name
            Nothing -> case semaParamList pl empty_st of
                Just nst -> let fs = pSFun ft pl
                                -- param list can cover function name
                                nst' = Map.union nst (Map.insert fn fs st)
                                -- insert ("", SFunction) to symbol table to ckeck ret_stmt
                            in case semaComdStmt fb sc (Map.insert "" (STempSymbol fn fs) nst') of
                                Just ast -> Just (FuncDef ft pf (Identifier p fn) pl ast, Map.insert fn fs st)
                                _        -> Nothing
                Nothing  -> Nothing
            _ -> putSemaError sc p "Confilcting definations for function" fn

    semaParamList :: [(Type, Ast)] -> SymbolTable -> Maybe SymbolTable
    semaParamList ((t, Identifier p i):pls) st = step >>= semaParamList pls where
        step = case Map.lookup i st of
            Nothing -> Just $ Map.insert i (SVariable $ semaType t) st
            _       -> putSemaError sc p "Confilcting definations for parameter" i
    semaParamList [] st = Just st

    pSFun :: FunType -> [(Type, Ast)] -> Symbol
    pSFun t pl = SFunction (semaFType t) (map semaType $ foldr step [] pl)
        where step (a, b) zero = a : zero


semaComdStmt :: Ast -> String -> SymbolTable -> Maybe Ast
semaComdStmt (ComdStmt p cs vs sl) sc st =
    semaConst cs sc st >>= semaVar vs sc >>= semaStmtList sl sc >>= rebuild where
        rebuild ast = Just $ ComdStmt p [] vs ast


semaStmtList :: Ast -> String -> SymbolTable -> Maybe Ast
semaStmtList (StmtList p stmts) sc st = toStmtList $ semaStmts stmts [] where
    toStmtList :: Maybe [Ast] -> Maybe Ast
    toStmtList (Just x) = Just $ StmtList p x
    toStmtList Nothing  = Nothing

    semaStmts :: [Ast] -> [Ast] -> Maybe [Ast]
    semaStmts [] r = Just r
    semaStmts (stmt:stmts) zero = semaAStmt stmt sc st </> semaStmts stmts zero


semaAStmt :: Ast -> String -> SymbolTable -> Maybe Ast
semaAStmt (IfStmt p c s es) sc st = toIfStmt $ case semaExpr c sc st of
    Just a -> Just a </> semaAStmt s sc st </> semaAStmt es sc st </> Just []
    _      -> Nothing
    where
        toIfStmt Nothing = Nothing
        toIfStmt (Just [a, b, c]) = Just $ IfStmt p a b c


semaAStmt sl@(StmtList _ _) sc st = semaStmtList sl sc st


semaAStmt (ForStmt p s e u b) sc st = toForStmt $ case semaExpr e sc st of
    Just a -> semaAStmt s sc st
           </> Just a
           </> semaAStmt u sc st
           </> semaAStmt b sc (Map.insert ".loop" SReserveSymbol st)
           </> Just []
    _      -> Nothing
    where
        toForStmt Nothing = Nothing
        toForStmt (Just [a, b, c, d]) = Just $ ForStmt p a b c d


semaAStmt (DoStmt p s c) sc st = toDoStmt $ case semaExpr c sc st of
    Just a  -> semaAStmt s sc (Map.insert ".loop" SReserveSymbol st)
            </> Just a
            </> Just []
    Nothing -> Nothing
    where
        toDoStmt Nothing = Nothing
        toDoStmt (Just [a, b]) = Just $ DoStmt p a b


semaAStmt (Rd p rd) sc st = toReadStmt $ for_rest rd where
    for_rest :: [Ast] -> Maybe [Ast]
    for_rest (Identifier p x : xs) = check p x </> for_rest xs
    for_rest [] = Just []

    check p x = case Map.lookup x st of
        Just (SVariable _)    -> Just $ Identifier p x
        Nothing               -> putSemaError sc p "Variable not in scope:" x
        Just (SArray SInt _)  -> putSemaError sc p
            ("Couldn't match excepted type " ++ srd_str "int" ++ "with actual type " ++ srd_str "int[]" ++ ":") x
        Just (SArray SChar _) -> putSemaError sc p
            ("Couldn't match excepted type " ++ srd_str "char" ++ "with actual type " ++ srd_str "char[]" ++ ":") x
        Just (SConst _ _)     -> putSemaError sc p "Couldn't read into a const:" x
        Just (SFunction _ _)  -> putSemaError sc p "Couldn't read into a function:" x
        _                     -> putSemaError sc p "Type error for" x

    toReadStmt Nothing = Nothing
    toReadStmt (Just a) = Just $ Rd p a


semaAStmt (Wt p s e) sc st = bind (semaFormatStr s) $ foldr step (Just []) e where
    step e z = case z of
        Nothing -> Nothing
        Just x  -> case semaExpr e sc st of
            Just a  -> Just $ a : x
            Nothing -> Nothing
    bind (Just a) (Just b) = Just $ Wt p a b
    bind _ _ = Nothing


semaAStmt (Ret p e) sc st = case semaExpr e sc st of
    Just x -> case Map.lookup "" st of
        Just (STempSymbol fn (SFunction SVoid _)) -> case x of
            Empty -> Just $ Ret p Empty
            _     -> putSemaError sc p "Return a none-void value in void function" fn
        Just (STempSymbol fn _) -> case x of
            Empty -> putSemaError sc p "Return a void value in none-void function" fn
            x     -> Just $ Ret p x
    Nothing -> Nothing


semaAStmt (FuncCall p (Identifier pi fn) pl) sc st = case Map.lookup fn st of
    Just (SFunction _ pld) -> toFuncCall $ foreach pld pl check
    _ -> putSemaError sc p "Function not in scope:" fn
    where
        foreach (d:ds) (x:xs) f = f d x </> foreach ds xs f
        foreach [] [] _ = Just []
        foreach _  [] _ = putSemaError sc p "Too few arguments to function" fn
        foreach [] _  _ = putSemaError sc p "Too many arguments to function" fn

        check d x = case semaExpr x sc st of
            Just a  -> Just a
            Nothing -> Nothing

        toFuncCall Nothing = Nothing
        toFuncCall (Just x) = Just $ FuncCall p (Identifier pi fn) x


semaAStmt Empty _ _ = Just Empty


semaAStmt (Assign p l r) sc st = case l of
    (Array _ _ _) -> case semaExpr l sc st of
        Just x  -> case semaExpr r sc st of
            Just a  -> Just $ Assign p x a
            Nothing -> Nothing
        Nothing -> Nothing
    (Identifier pi i) -> case Map.lookup i st of
        Just (SVariable _) -> case semaExpr r sc st of
            Just a  -> Just $ Assign p l a
            Nothing -> Nothing
        Nothing -> putSemaError sc pi "Variable not in scope:" i


semaAStmt bk@(Break p) sc st = case Map.lookup ".loop" st of
    Just _ -> Just bk
    _      -> putSemaError sc p "Statement not within loop:" "break"


semaAStmt ct@(Continue p) sc st = case Map.lookup ".loop" st of
    Just _ -> Just ct
    _      -> putSemaError sc p "Statement not within loop:" "continue"


-- check format string is boring, i don't want to do it...
semaFormatStr :: Ast -> Maybe Ast
semaFormatStr a = Just a


semaExpr :: Ast -> String -> SymbolTable -> Maybe Ast
semaExpr (BinNode o p l r) sc st = toExpr o (semaExpr l sc st) (semaExpr r sc st)
    where
        toExpr o a b = case (a, b) of
            (Nothing, _)     -> Nothing
            (_, Nothing)     -> Nothing
            (Just a, Just b) -> Just $ BinNode o p a b


semaExpr (UnaryNode op p a) sc st = case semaExpr a sc st of
    Just x  -> Just $ UnaryNode op p x
    Nothing -> Nothing


semaExpr n@(Number _ _) _ _ = Just n


semaExpr c@(Ch _ _) _ _ = Just c


semaExpr (Array pa (Identifier pi n) i) sc st = case Map.lookup n st of
    Just (SArray _ _) -> case semaExpr i sc st of
        Just a  -> Just $ Array pa (Identifier pi n) a
        Nothing -> Nothing
    _ -> putSemaError sc pi "Variable not in scope:" n


semaExpr ident@(Identifier p i) sc st = case Map.lookup i st of
    Just x@(SVariable _)   -> Just ident
    Just x@(SConst _  _)   -> Just ident
    Just x@(SArray _  _)   -> putSemaError sc p
        ("Couldn't match excepted type " ++ srd_str "int/char" ++ "with actual type " ++ srd_str "array" ++ ":") i
    Just x@(SFunction _ _) -> putSemaError sc p
        ("Couldn't match excepted type " ++ srd_str "int/char" ++ "with actual type " ++ srd_str "function" ++ ":") i
    Nothing                -> putSemaError sc p "Variable not in scope:" i


semaExpr fc@(FuncCall pf (Identifier p fn) pl) sc st = case Map.lookup fn st of
    Just (SFunction SVoid  _) -> putSemaError sc pf "Call a void function in expression:" fn
    Just (SFunction _ _)      -> semaAStmt fc sc st
    _                         -> putSemaError sc p "Function not in scope:" fn

semaExpr Empty _ _ = Just Empty


semaType :: Type -> SType
semaType TInt  = SInt
semaType TChar = SChar


semaFType :: FunType -> SType
semaFType FInt  = SInt
semaFType FChar = SChar
semaFType FVoid = SVoid


-- help functions
srd_str s = "‘" ++ s ++ "’"
putSemaError inp (a, b) e i = let n = length $ show a in error $ "\n" ++
    show a ++ ":" ++ show b ++ ": \x1b[91msemantic error\x1b[0m:\n - " ++ e ++ " " ++ srd_str i ++ "\n" ++
    replicate n ' ' ++ " \x1b[94m|\n" ++
    show a ++ " | \x1b[0m" ++ lines inp !! (a - 1) ++ "\n" ++
    replicate n ' ' ++ " \x1b[94m|\x1b[91m " ++ replicate (b - 1) ' ' ++ "^\x1b[0m"

-- instance (:) for Maybe
infixr 5 </>
(</>) :: Maybe a -> Maybe [a] -> Maybe [a]
a </> b = case b of
    Nothing -> Nothing
    Just xs -> case a of
        Nothing -> Nothing
        Just  x -> Just (x:xs)

