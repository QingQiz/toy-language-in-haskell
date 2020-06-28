module Simplify where

import Ast
import Functions


simplify :: Ast -> Ast
simplify = untilNoChange (mapToExpr afterTrans)
         . untilNoChange (mapToExpr (transExpr . tryMerge))
         . untilNoChange (mapToExpr (tryApply . transExpr))
         . untilNoChange (mapToExpr expand)


mapToExpr :: (Ast -> Ast) -> Ast -> Ast
mapToExpr f (BinNode o _ l r) = f $ pBin o (mapToExpr f l) (mapToExpr f r)
mapToExpr f (UnaryNode o _ a) = f $ pUnary o $ mapToExpr f a
mapToExpr f x = f x


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


notOp op = case op of {
        Neq -> Equ; Equ -> Neq; Gt -> LE; Ls -> GE; GE -> Ls; LE -> Gt; _ -> op
    }


negOp op = case op of {
        Gt -> Ls; Ls -> Gt; GE -> LE; LE -> GE; _ -> op
    }


findFactor :: Ast -> Ast -> Either Ast Ast
findFactor e t = case e of
    BinNode Mul _ a b -> case findFactor a t of
        Left _   -> case findFactor b t of
            Left _   -> Left e
            Right b' -> Right $ pMul a b'
        Right a' -> Right $ pMul a' b
    Number _ x     | pNum x == t -> Right $ pNum 1
                   | pNum (negate x) == t -> Right $ pNum (-1)
    Identifier _ x | pId  x == t -> Right $ pNum 1
    _ -> Left e


findAllFactor :: Ast -> [(Ast, Ast)]
findAllFactor e = foldr step [] (find e []) where
    find (BinNode Mul _ a b) xs = find b (find a xs)
    find (Number _ x) xs = pNum x : xs
    find (Identifier _ x) xs = pId x : xs
    find _ xs = xs

    step x z = case findFactor e x of
        Right x' -> (x, x'):z
        Left _   -> z


fromEither fall_back either = case either of
    Right x -> x
    Left  _ -> fall_back


expand :: Ast -> Ast
-- n * (x ± b) = n * x ± n * b
expand e@(BinNode Mul _ (Number _ n) exp) = case exp of
    BinNode op _ a b | op `elem` [Add, Sub] -> pBin op (pMul a $ pNum n) (pMul b $ pNum n)
    BinNode op _ a b | op `elem` [Add, Sub] -> pBin op (pMul a $ pNum n) (pMul b $ pNum n)
    _ -> e
expand e@(BinNode Mul _ exp (Number _ n)) = case exp of
    BinNode op _ a b | op `elem` [Add, Sub] -> pBin op (pMul a $ pNum n) (pMul b $ pNum n)
    BinNode op _ a b | op `elem` [Add, Sub] -> pBin op (pMul a $ pNum n) (pMul b $ pNum n)
    _ -> e
expand e@(BinNode Mul _ (Identifier _ n) exp) = case exp of
    BinNode op _ a b | op `elem` [Add, Sub] -> pBin op (pMul a $ pId n) (pMul b $ pId n)
    BinNode op _ a b | op `elem` [Add, Sub] -> pBin op (pMul a $ pId n) (pMul b $ pId n)
    _ -> e
expand e@(BinNode Mul _ exp (Identifier _ n)) = case exp of
    BinNode op _ a b | op `elem` [Add, Sub] -> pBin op (pMul a $ pId n) (pMul b $ pId n)
    BinNode op _ a b | op `elem` [Add, Sub] -> pBin op (pMul a $ pId n) (pMul b $ pId n)
    _ -> e
expand e = e


-- merge n * exp' n * f -> n (f + exp')
merge :: Ast -> Ast -> Ast -> Either Ast Ast
merge exp f a@(Number _ x) = case exp of
    BinNode Mul _ (Number _ n) e' | n == x        -> Right $ pMul (pAdd e' f) (pNum x)
                                  | n == negate x -> Right $ pMul (pSub e' f) (pNum n)
    BinNode Mul _ e' (Number _ n) | n == x        -> Right $ pMul (pAdd e' f) (pNum x)
                                  | n == negate x -> Right $ pMul (pSub e' f) (pNum n)
    BinNode op _ l r | op `elem` [Add, Sub] ->
        case merge l f a of
            Left _ -> case merge r f (if op == Add then a else (pNum $ negate x)) of
                Left _   -> Left exp
                Right r' -> Right $ pBin op l r'
            Right l' -> Right $ pBin op l' r
    UnaryNode Neg _ a -> case merge a f (pNum $ negate x)  of
        Right a' -> Right $ pNeg a'
        Left _   -> Left exp
    BinNode Mul _ _ _ ->
        let fc a n = if x `mod` n == 0
                then case merge a f (pNum $ x `div` n)  of
                    Left _   -> Left exp
                    Right a' -> Right $ pMul a' $ pNum n
                else Left exp
        in case exp of
            BinNode _ _ a (Number _ n) -> fc a n
            BinNode _ _ (Number _ n) a -> fc a n
            BinNode _ _ a b -> case findFactor exp (pNum x) of
                Right e -> Right $ pMul (pAdd e f) (pNum x)
                Left _  -> Left exp
    e -> Left e

merge exp f a@(Identifier _ x) = case exp of
    BinNode Mul _ (Identifier _ n) e' | n == x -> Right $ pMul (pAdd e' f) (pId x)
    BinNode Mul _ e' (Identifier _ n) | n == x -> Right $ pMul (pAdd e' f) (pId x)
    BinNode op _ l r | op `elem` [Add, Sub] ->
        case merge l f a of
            Left _ -> case merge r (if op == Add then f else pNeg f) a of
                Left _   -> Left exp
                Right r' -> Right $ pBin op l r'
            Right l' -> Right $ pBin op l' r
    UnaryNode Neg _ e -> case merge e (pNeg f) a  of
        Right e' -> Right $ pNeg e'
        Left _   -> Left exp
    BinNode Mul _ _ _ -> case findFactor exp (pId x) of
        Right e -> Right $ pMul (pAdd e f) (pId x)
        Left _  -> Left exp
    e -> Left e


tryMerge :: Ast -> Ast
tryMerge e@(BinNode op _ a b) | op `elem` [Add, Sub] =
    case testUntilRight a (findAllFactor b) of
        Empty -> case testUntilRight b (findAllFactor a) of
            Empty -> e
            x     -> if op == Add then x else pNeg x
        x     -> x
    where
        testUntilRight t ((x, y):xs) = case merge t (if op == Add then y else pNeg y) x of
            Right t' -> t'
            Left  _  -> testUntilRight t xs
        testUntilRight _ [] = Empty
tryMerge e = e


afterTrans :: Ast -> Ast
-- (a op b) bool-op 0 = a bool-op b
afterTrans e@(BinNode op _ a (Number _ 0)) | op `elem` [Neq, Equ, Gt, Ls, GE, LE] = case a of
        BinNode Add _ a b -> case apply a Mul (-1, "") of
            Right x -> pBin (negOp op) x b
            Left _  -> pBin op a $ simplify $ pNeg b
        BinNode Sub _ a b -> pBin op a b
        _ -> e
afterTrans e = e


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
transExpr e@(BinNode Sub _ (Identifier _ a) (Identifier _ b)) | a == b = pNum 0
-- a && a = a
transExpr (BinNode And _ (Identifier _ a) (Identifier _ b)) | a == b = arithToBool (pId a)
-- a && 0 = 0
-- a && 1 = a
transExpr (BinNode And _ a (Number _ n)) | n /= 0 = arithToBool a
                                         | n == 0 = pNum 0
transExpr (BinNode And _ (Number _ n) a) | n /= 0 = arithToBool a
                                         | n == 0 = pNum 0
-- a || a = a
transExpr (BinNode Or _ (Identifier _ a) (Identifier _ b)) | a == b = arithToBool (pId a)
-- a || 0 = a
-- a || 1 = 1
transExpr (BinNode And _ a (Number _ n)) | n == 0 = arithToBool a
                                         | n /= 0 = pNum 1
transExpr (BinNode And _ (Number _ n) a) | n == 0 = arithToBool a
                                         | n /= 0 = pNum 1
-- a + (c ± d) = a + c ± d
transExpr (BinNode Add _ a (BinNode op _ b c)) | op `elem` [Add, Sub] = pBin op (pAdd a b) c
-- a - (c ± d) = a - c ∓ d
transExpr (BinNode Sub _ a (BinNode Add _ b c)) = pSub (pSub a b) c
transExpr (BinNode Sub _ a (BinNode Sub _ b c)) = pAdd (pSub a b) c
-- a * (b * c) = a * b * c
transExpr (BinNode Mul _ a (BinNode Mul _ b c)) = pMul (pMul a b) c
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
-- -a op 0 = a (-op) 0
transExpr (BinNode op _ (UnaryNode Neg _ a) (Number _ 0))
    | op `elem` [Neq, Equ, Gt, Ls, GE, LE] = pBin (negOp op) a (pNum 0)
transExpr (BinNode op _ (Number _ 0) (UnaryNode Neg _ a))
    | op `elem` [Neq, Equ, Gt, Ls, GE, LE] = pBin (negOp op) (pNum 0) a
-- a * n op x = a (op / n) (x / n)
transExpr e@(BinNode op _ (BinNode Mul _ a b) (Number _ x))
    | op `elem` [Neq, Equ, Gt, Ls, GE, LE] = case (a, b) of
        (Number _ n, _) | test n && n > 0 -> pBin op b (next n)
                        | test n && n < 0 -> pBin (negOp op) b (next n)
        (_, Number _ n) | test n && n > 0 -> pBin op a (next n)
                        | test n && n < 0 -> pBin (negOp op) a (next n)
        _ -> e
    where test n = x `mod` n == 0; next n = pNum $ x `div` n
transExpr e@(BinNode op _ (Number _ x) (BinNode Mul _ a b))
    | op `elem` [Neq, Equ, Gt, Ls, GE, LE] = case (a, b) of
        (Number _ n, _) | test n && n > 0 -> pBin op (next n) b
                        | test n && n < 0 -> pBin (negOp op) (next n) b
        (_, Number _ n) | test n && n > 0 -> pBin op (next n) a
                        | test n && n < 0 -> pBin (negOp op) (next n) a
        _ -> e
    where test n = x `mod` n == 0; next n = pNum $ x `div` n
-- a bool-op b = (a + Neg b) bool-op 0
transExpr (BinNode op _ a b) | op `elem` [Neq, Equ, Gt, Ls, GE, LE] = pBin op (pSub a b) (pNum 0)
-- not expr
transExpr (UnaryNode Not _ (BinNode op _ a b))
    -- !(a op b) = a (!op) b
    | op `elem` [Neq, Equ, Gt, Ls, GE, LE] = pBin (notOp op) a b
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
-- -(a - b) = b - a
transExpr (UnaryNode Neg _ (BinNode Sub _ a b)) = pSub b a
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
        Right a' -> Right $ pNeg a'
        Left _   -> Left e
    -- apply a*id (+) n*id = (a+n)*id
    e@(BinNode Mul _ a (Identifier _ name')) | name' == name -> Right $ pMul (pAdd a $ pNum x) $ pId name
    e@(BinNode Mul _ (Identifier _ name') a) | name' == name -> Right $ pMul (pAdd a $ pNum x) $ pId name
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
            _ -> Left e
    e -> Left e

apply exp Mul t@(x, "") = case exp of
    -- apply n (*) x -> n * x
    Number _ n -> Right $ pNum $ x * n
    -- apply (a - b) (*) -1 -> b - a
    BinNode Sub _ a b | x == -1 -> Right $ pSub b a
    -- apply (a + n) (*) -1 -> (-n) - a
    BinNode Add _ (Number _ n) a -> Right $ pSub (pNum $ negate n) a
    BinNode Add _ a (Number _ n) -> Right $ pSub (pNum $ negate n) a
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
            _ -> Left e
    -- apply (-a) (*) x -> apply a (*) (-x)
    e@(UnaryNode Neg _ a) -> apply a Mul (negate x, "")
    e -> Left e


tryApply :: Ast -> Ast
-- a +/- n -> apply a (+) (n/-n)
tryApply e@(BinNode op _ a (Number _ n)) | op `elem` [Add, Sub] =
    fromEither e $ apply a Add (if op == Add then n else negate n, "")
-- n + a -> apply a (+) n
tryApply e@(BinNode Add _ (Number _ n) a) = fromEither e $ apply a Add (n, "")
-- n - a -> -(apply a (+) -n)
tryApply e@(BinNode Sub _ (Number _ n) a) = case apply a Add (negate n, "") of
    Right a' -> pNeg a'
    _        -> e
-- a * n -> apply a (*) n
tryApply e@(BinNode Mul _ a (Number _ n)) = fromEither e $ apply a Mul (n, "")
tryApply e@(BinNode Mul _ (Number _ n) a) = fromEither e $ apply a Mul (n, "")
-- -a -> apply a (*) -1
tryApply e@(UnaryNode Neg _ a) = fromEither e $ apply a Mul (-1, "")
-- a +/- id -> apply a (+) id/-id
tryApply e@(BinNode op _ a b) | op `elem` [Add, Sub] = case (a, b) of
    (_, Identifier _ name) -> fromEither e $ apply a Add (if op == Add then 1 else -1, name)
    (Identifier _ name, _) -> case apply b Add (if op == Add then 1 else -1, name) of
        Right b' -> if op == Add then b' else pNeg b'
        _        -> e
    (_, BinNode Mul _ (Number _ n) (Identifier _ name)) ->
        fromEither e $ apply a Add (if op == Add then n else negate n, name)
    (_, BinNode Mul _ (Identifier _ name) (Number _ n)) ->
        fromEither e $ apply a Add (if op == Add then n else negate n, name)
    (BinNode Mul _ (Number _ n) (Identifier _ name), _) ->
        case apply b Add (if op == Add then n else negate n, name) of
            Right b' -> if op == Add then b' else pNeg b'
            _        -> e
    (BinNode Mul _ (Identifier _ name) (Number _ n), _) ->
        case apply b Add (if op == Add then n else negate n, name) of
            Right b' -> if op == Add then b' else pNeg b'
            _        -> e
    _ -> e
tryApply e = e

