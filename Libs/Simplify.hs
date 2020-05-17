module Simplify where

import Ast
import Parser
import Grammar


simplify :: Ast -> Ast
simplify = untilNoChange (mapToExpr (tryApply . transExpr))


mapToExpr :: (Ast -> Ast) -> Ast -> Ast
mapToExpr f (BinNode o _ l r) = f $ pBin o (mapToExpr f l) (mapToExpr f r)
mapToExpr f (UnaryNode o _ a) = f $ pUnary o $ mapToExpr f a
mapToExpr f x = f x


untilNoChange :: Eq a => (a -> a) -> a -> a
untilNoChange f x = until_no_change' f (f x) x where
    until_no_change' f x x' = if x == x' then x else until_no_change' f (f x) x


-- transform arith-expr to bool-expr, e.g. a - b ==> a != b
arithToBool :: Ast -> Ast
arithToBool e = case e of
    Number _ n -> pNum $ fromEnum (n/=0)
    BinNode Sub _ l r -> pBin Neq l r
    BinNode Mul _ (Number _ n) r -> if n == 0 then pNum 0 else pBin Neq r (pNum 0)
    BinNode Mul _ l (Number _ n) -> if n == 0 then pNum 0 else pBin Neq l (pNum 0)
    BinNode op _ _ _
        | op `elem` [Gt, Ls, GE, LE, Equ, Neq, And, Or] -> e
        | otherwise -> pBin Neq e (pNum 0)
    _ -> pBin Neq e (pNum 0)


calc :: Int -> Op -> Int -> Int
calc a op b = let f = fromEnum in
    case op of {
        Mul -> a * b      ; Div -> a `div` b  ; Add -> a + b     ; Sub -> a - b     ;
        Gt  -> f $ a > b  ; Ls  -> f $ a < b  ; GE -> f $ a >= b ; LE -> f $ a <= b ;
        Equ -> f $ a == b ; Neq -> f $ a /= b ;
        And -> f $ a /= 0 && b /= 0 ;
        Or  -> f $ a /= 0 || b /= 0 }


transExpr :: Ast -> Ast
-- calculate const
transExpr (BinNode o _ (Number _ n) (Number _ n')) = pNum $ calc n o n'
-- expr * 0 = 0
transExpr (BinNode Mul _ e (Number _ 0)) = pNum 0
transExpr (BinNode Mul _ (Number _ 0) e) = pNum 0
-- expr * 1 = a
transExpr (BinNode Mul _ e (Number _ 1)) = e
transExpr (BinNode Mul _ (Number _ 1) e) = e
-- expr * (-1) = Neg expr
transExpr (BinNode Mul _ e (Number _ (-1))) = pNeg e
transExpr (BinNode Mul _ (Number _ (-1)) e) = pNeg e
-- id / id = 1
transExpr e@(BinNode Div _ (Identifier _ a) (Identifier _ b)) | a == b = pNum 1
-- 0 / expr = 0
transExpr (BinNode Div _ (Number _ 0) e) = pNum 0
-- expr / 1 = expr
transExpr (BinNode Div _ e (Number _ 1)) = e
-- expr + 0 = expr
transExpr (BinNode Add _ e (Number _ 0)) = e
transExpr (BinNode Add _ (Number _ 0) e) = e
-- id + id = 2 * id
transExpr (BinNode Add _ (Identifier _ x) (Identifier _ x')) | x == x' = pMul (pId x) (pNum 2)
-- expr - 0 = 0
transExpr (BinNode Sub _ e (Number _ 0)) = e
transExpr (BinNode Sub _ (Number _ 0) e) = pNeg e
-- id - id = 0
transExpr e@(BinNode Sub _ (Identifier _ a) (Identifier _ b)) = if a == b then pNum 0 else e
-- a + (c ± d) = a + c ± d
transExpr (BinNode Add _ a (BinNode op _ b c)) | op `elem` [Add, Sub] = pBin op (pAdd a b) c
-- a - (c ± d) = a - c ∓ d
transExpr (BinNode Sub _ a (BinNode Add _ b c)) = pSub (pSub a b) c
transExpr (BinNode Sub _ a (BinNode Sub _ b c)) = pAdd (pSub a b) c
-- a * (b * c) = a * b * c
transExpr (BinNode Mul _ a (BinNode Mul _ b c)) = pMul (pMul a b) c
-- id * a ± id * b = id * (a ± b)
transExpr e@(BinNode op _ (BinNode Mul _ a b) (BinNode Mul _ c d)) | op `elem` [Add, Sub] =
    case (a, b, c, d) of
        (Identifier _ x, y, Identifier _ x', y') | x == x' -> pMul (pBin op y y') (pId x)
        (Identifier _ x, y, y', Identifier _ x') | x == x' -> pMul (pBin op y y') (pId x)
        (y, Identifier _ x, Identifier _ x', y') | x == x' -> pMul (pBin op y y') (pId x)
        (y, Identifier _ x, y', Identifier _ x') | x == x' -> pMul (pBin op y y') (pId x)
-- a + (-b) = a - b
transExpr (BinNode Add _ a (UnaryNode Neg _ b)) = pSub a b
-- a - (-b) = a + b
transExpr (BinNode Sub _ a (UnaryNode Neg _ b)) = pAdd a b
-- - a + b = b - a
transExpr (BinNode Add _ (UnaryNode Neg _ a) b) = pSub b a
-- - a - b = -(a + b)
transExpr (BinNode Sub _ (UnaryNode Neg _ a) b) = pNeg (pAdd a b)
-- (!a&&!b) = !(a||b)
transExpr (BinNode And _ (UnaryNode Not _ a) (UnaryNode Not _ b)) = pNot $ pBin Or  a b
-- (!a||!b) = !(a&&b)
transExpr (BinNode Or  _ (UnaryNode Not _ a) (UnaryNode Not _ b)) = pNot $ pBin And a b
-- bool-expr != 0 = bool-expr
transExpr (BinNode Neq _ (Number _ 0) e@(BinNode op _ _ _)) | op `elem` [Gt, Ls, GE, LE, Equ, Neq, And, Or] = e
transExpr (BinNode Neq _ e@(BinNode op _ _ _) (Number _ 0)) | op `elem` [Gt, Ls, GE, LE, Equ, Neq, And, Or] = e
transExpr (BinNode Neq _ (Number _ 0) e@(UnaryNode Not _ _)) = e
transExpr (BinNode Neq _ e@(UnaryNode Not _ _) (Number _ 0)) = e

-- -(a - b) = b - a
transExpr (UnaryNode Neg _ (BinNode Sub _ a b)) = pSub b a
-- not expr
transExpr (UnaryNode Not _ (BinNode op _ a b))
    -- !(a op b) = a (!op) b
    | op `elem` [Neq, Equ, Gt, Ls, GE, LE] =
        let op' = case op of {
                Neq -> Equ; Equ -> Neq; Gt -> LE; Ls -> GE; GE -> Ls; LE -> Gt; _ -> op
            }
        in pBin op' a b
    -- !(a && !b) = !a || b, !(a || !b) = !a && b
    | op `elem` [And, Or] = let op' = if op == And then Or else And in
        case (a, b) of
            (UnaryNode Not _ a', _) -> pBin op' a' (pNot b)
            (a, UnaryNode Not _ b') -> pBin op' (pNot a) b'
    -- !(a-b) = (a == b)
    | otherwise =
        transExpr $ pNot $ arithToBool $ pBin op a b
-- -(-a) = a
transExpr (UnaryNode Neg _ (UnaryNode Neg _ a)) = a
-- !(!a) = (bool)a
transExpr (UnaryNode Not _ (UnaryNode Not _ a)) = arithToBool a
-- !(-a) = !a
transExpr (UnaryNode Not _ (UnaryNode Neg _ a)) = pNot a
-- Neg n = Number (-n)
transExpr (UnaryNode Neg _ (Number _ n)) = pNum $ negate n
-- !n = Number (n==0)
transExpr (UnaryNode Not _ (Number _ n)) = pNum $ fromEnum (n == 0)
transExpr e = e


apply :: Ast -> Op -> (Int, String) -> Either Ast Ast
apply exp Add t@(x, name) = case exp of
    -- apply n (+) n' -> n + n'
    Number _ n | name == "" -> Right $ pNum $ n + x
    -- apply id (+) n*id -> (n + 1) * id
    Identifier _ name' | name == name' -> Right $ pMul (pId name) (pNum $ x + 1)
    -- apply (a + b) (+) n -> (apply a (+) n) + b | a + (apply b (+) n) | a + b
    -- apply (a - b) (+) n -> (apply a (+) n) - b | a - (apply b (+) (-n)) | a - b
    e@(BinNode op _ l r) | op `elem` [Add, Sub] ->
        case apply l Add t of
            Left _ -> case apply r Add (if op == Add then t else (negate x, name)) of
                Left _   -> Left e
                Right r' -> Right $ pBin op l r'
            Right l' -> Right $ pBin op l' r
    -- apply (-a) (+) n -> -(apply a (+) (-n))
    e@(UnaryNode Neg _ a) -> case apply a Add (negate x, name) of
        Left _ -> Left e
        Right a' -> Right $ pNeg a'
    -- apply n*b (+) n' -> n * (apply b (+) (n' `div` n)) | n * b
    e@(BinNode Mul _ _ _) ->
        let f a n = if x `mod` n == 0
            then case apply a Add (x `div` n, name) of
                Left _   -> Left e
                Right a' -> Right $ pMul a' $ pNum n
            else Left e
        in case e of
            BinNode _ _ a (Number _ n) -> f a n
            BinNode _ _ (Number _ n) a -> f a n
    e -> Left e

apply exp Mul t@(x, "") = case exp of
    -- apply n (*) x -> n * x
    Number _ n -> Right $ pNum $ x * n
    -- apply (a - b) (*) -1 -> b - a
    BinNode Sub _ a b | x == -1 -> Right $ pSub b a
    -- apply (-a) (*) -1 -> a
    UnaryNode Neg _ a | x == -1 -> Right a
    -- apply (a * b) (*) x -> (apply a (*) x) * b | a * (apply b (*) x) | a * b
    e@(BinNode Mul _ a b) -> case apply a Mul t of
        Left _ -> case apply b Mul t of
            Left _ -> Left e
            Right b' -> Right $ pMul a b'
        Right a' -> Right $ pMul a' b
    -- apply (a ± n) (*) x -> (apply a (*) x) ± (n * x)
    e@(BinNode op _ _ _) | op `elem` [Add, Sub] ->
        let
            f a n = case apply a Mul t of
                Left _   -> Left e
                Right a' -> Right $ pBin op a' $ pNum (n * x)
        in case e of
            BinNode _ _ (Number _ n) a -> f a n
            BinNode _ _ a (Number _ n) -> f a n
    -- apply (-a) (*) x -> apply a (*) (-x)
    e@(UnaryNode Neg _ a) -> apply a Mul (negate x, "")
    e -> Left e


tryApply :: Ast -> Ast
tryApply exp = case exp of
    -- a +/- n -> apply a (+) (n/-n)
    BinNode op _ a (Number _ n) | op `elem` [Add, Sub] ->
        case apply a Add (if op == Add then n else negate n, "") of
            Right a' -> a'
            _        -> exp
    -- n + a -> apply a (+) n
    BinNode Add _ (Number _ n) a -> case apply a Add (n, "") of
        Right a' -> a'
        _        -> exp
    -- n - a -> -(apply a (+) -n)
    BinNode Sub _ (Number _ n) a -> case apply a Add (negate n, "") of
        Right a' -> pNeg a'
        _        -> exp
    -- a * n -> apply a (*) n
    BinNode Mul _ a (Number _ n) -> case apply a Mul (n, "") of
        Right a' -> a'
        _        -> exp
    BinNode Mul _ (Number _ n) a -> case apply a Mul (n, "") of
        Right a' -> a'
        _        -> exp
    UnaryNode Neg _ a -> case apply a Mul (-1, "") of
        Right a' -> a'
        _        -> exp
    -- a +/- id -> apply a (+) id/-id
    BinNode op _ a b | op `elem` [Add, Sub] -> case (a, b) of
        (_, Identifier _ name) -> case apply a Add (if op == Add then 1 else -1, name) of
            Right a' -> a'
            _        -> exp
        (Identifier _ name, _) -> case apply a Add (if op == Add then 1 else -1, name) of
            Right a' -> if op == Add then a' else pNeg a'
            _        -> exp
        (_, BinNode Mul _ (Number _ n) (Identifier _ name)) ->
            case apply a Add (if op == Add then n else negate n, name) of
                Right a' -> a'
                _        -> exp
        (_, BinNode Mul _ (Identifier _ name) (Number _ n)) ->
            case apply a Add (if op == Add then n else negate n, name) of
                Right a' -> a'
                _        -> exp
        (BinNode Mul _ (Number _ n) (Identifier _ name), _) ->
            case apply a Add (if op == Add then n else negate n, name) of
                Right a' -> if op == Add then a' else pNeg a'
                _        -> exp
        (BinNode Mul _ (Identifier _ name) (Number _ n), _) ->
            case apply a Add (if op == Add then n else negate n, name) of
                Right a' -> if op == Add then a' else pNeg a'
                _        -> exp
        _ -> exp
    _ -> exp

