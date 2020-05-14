module Simplify where

import Ast
import Parser
import Grammar


transExpr :: Ast -> Ast
-- calculate const
transExpr (BinNode o _ (Number _ n) (Number _ n')) = pNum $ calc n o n'
-- expr * 0 = 0
transExpr (BinNode Mul _ e (Number _ 0)) = pNum 0
transExpr (BinNode Mul _ (Number _ 0) e) = pNum 0
-- expr * 1 = a
transExpr (BinNode Mul _ e (Number _ 1)) = e
transExpr (BinNode Mul _ (Number _ 1) e) = e
-- id * 2 = id + id
transExpr (BinNode Mul _ (Identifier _ a) (Number _ 2)) = pAdd (pId a) (pId a)
transExpr (BinNode Mul _ (Number _ 2) (Identifier _ a)) = pAdd (pId a) (pId a)
-- id / id = 1
transExpr e@(BinNode Div _ (Identifier _ a) (Identifier _ b)) = if a == b then pNum 1 else e
-- 0 / expr = 0
transExpr (BinNode Div _ (Number _ 0) e) = pNum 0
-- expr / 1 = expr
transExpr (BinNode Div _ e (Number _ 1)) = e
-- expr + 0 = expr
transExpr (BinNode Add _ e (Number _ 0)) = e
transExpr (BinNode Add _ (Number _ 0) e) = e
-- expr - 0 = 0
transExpr (BinNode Sub _ e (Number _ 0)) = e
transExpr (BinNode Sub _ (Number _ 0) e) = pNeg e
-- id - id = 0
transExpr e@(BinNode Sub _ (Identifier _ a) (Identifier _ b)) = if a == b then pNum 0 else e
-- a + (c ± d) = a + c ± d
transExpr (BinNode Add _ a (BinNode Add _ b c)) = pAdd (pAdd a b) c
transExpr (BinNode Add _ a (BinNode Sub _ b c)) = pAdd (pAdd a b) c
-- a - (c ± d) = a - c ∓ d
transExpr (BinNode Sub _ a (BinNode Add _ b c)) = pSub (pSub a b) c
transExpr (BinNode Sub _ a (BinNode Sub _ b c)) = pAdd (pSub a b) c
-- a * (b * c) = a * b * c
transExpr (BinNode Mul _ a (BinNode Mul _ b c)) = pMul (pMul a b) c
-- id * a ± id * b = id * (a + b)
transExpr e@(BinNode Add _ (BinNode Mul _ a b) (BinNode Mul _ c d)) = case (a, b, c, d) of
    (Identifier _ x, y, Identifier _ x', y') -> if x == x' then pMul (pAdd y y') (pId x) else e
    (Identifier _ x, y, y', Identifier _ x') -> if x == x' then pMul (pAdd y y') (pId x) else e
    (y, Identifier _ x, Identifier _ x', y') -> if x == x' then pMul (pAdd y y') (pId x) else e
    (y, Identifier _ x, y', Identifier _ x') -> if x == x' then pMul (pAdd y y') (pId x) else e
transExpr e@(BinNode Sub _ (BinNode Mul _ a b) (BinNode Mul _ c d)) = case (a, b, c, d) of
    (Identifier _ x, y, Identifier _ x', y') -> if x == x' then pMul (pSub y y') (pId x) else e
    (Identifier _ x, y, y', Identifier _ x') -> if x == x' then pMul (pSub y y') (pId x) else e
    (y, Identifier _ x, Identifier _ x', y') -> if x == x' then pMul (pSub y y') (pId x) else e
    (y, Identifier _ x, y', Identifier _ x') -> if x == x' then pMul (pSub y y') (pId x) else e
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
transExpr e = e


simplify :: Ast -> Ast
simplify u@(UnaryNode _ _ _) = untilNoChange (mapToExpr transExpr) u
simplify e@(BinNode _ _ _ _) = e -- TODO


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


pexpr = (\(Right x) -> x) . (parse expr)
testx = (\(Right x) -> x) $ parse expr "1-(1-1)-(1-a*2+1/3)+5*10*a+1+(1+1+1)+1"

