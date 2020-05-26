module Optimizer where

import CFG
import Register
import Functions
import Data.Char
import Data.List
import Data.List.Split
import Data.List.Utils
import qualified Data.Map as Map


-- doLocalOptimize :: CFG -> CFG
doGlobalOptimize cfg =
    let
        (ids, bbs_org) = unzip $ Map.toList $ getBasicBlocks cfg
        fs = splitWithFunction bbs_org
    in
        -- concat $ concat $ map globalOptimizeOnAFunction fs
        map globalOptimizeOnAFunction fs
    where
        splitWithFunction bbs = foldr step [] bbs where
            step b z@(x:xs) = case getEntry b of
                [] -> [b]:z
                _  -> (b:x):xs
            step b [] = [[b]]


globalOptimizeOnAFunction bbs = let (tac, size) = unzip $ map (toTAC . getCode) bbs in
    (map commonSubexprElim tac, size)


globalDeadCodeElim tac = undefined


commonSubexprElim tac = untilNoChange (\x -> csElim x [] (Map.fromList []) (Map.fromList [])) tac where
    csElim (c:cs) code z eq =
        csElim cs (doReplace c z eq : code) (updateZ c z) (updateEQ c eq)
    csElim [] code _ _ = reverse code

    updateZ c z =
        if snd c == "" || isLetter (head $ fst c)
        then z
        else if not $ isSimple (snd c) then Map.insert (snd c) (fst c) z else z

    updateEQ c z =
        if snd c == "" || '%' `notElem` fst c || isLetter (head $ snd c) || "%rbp" `isPrefixOf` (fst c)
        then z
        else if isSimple (snd c) then Map.insert (fst c) (snd c) z else z

    doReplace x@(_, "") _ _ = x
    doReplace c z eq = if '%' `notElem` fst c then c else
        -- common subexpression elimination
        case Map.lookup (snd c) z of
            Nothing -> if isRegGroup (snd c) && head (snd c) == '*'
                then case Map.lookup (tail (snd c)) z of
                    -- copy propagation
                    Nothing -> (fst c, doCopy (snd c) eq)
                    Just  x -> (fst c, "*" ++ x)
                -- copy propagation
                else (fst c, doCopy (snd c) eq)
            Just  x -> (fst c, x)

    doCopy c eq = case getOperand c of
        (a, "", _) -> case copy a eq of
            Nothing -> if isRegGroup a
                then let (h:val) = getGroupVal a in h ++ func val []
                else a
            Just  x -> x
        (a, b, op) -> doCopy a eq ++ op ++ doCopy b eq
        where
            func (c:cs) z = case copy c eq of
                Nothing -> func cs (c:z)
                Just  x -> if isRegGroup x then func cs (c:z) else func cs (x:z)
            func [] z = "(" ++ intercalate "," (reverse z) ++ ")"

    copy a eq = if a == "" || "%rsp" `isPrefixOf` a then Nothing else
        case Map.lookup a eq of
            Nothing -> if head a == '*'
                then case Map.lookup (tail a) eq of
                    Nothing -> Nothing
                    Just  x -> Just $ '*':x
                else Nothing
            Just  x -> Just x


getGroupVal rg = init $ concat $ map (splitOn ")") $ concat $ map (splitOn "(") $ splitOn "," rg

isSimple c = not (isRegGroup c) && case break (\x -> x `elem` "+-*/") (tail $ tail c) of
    ("", _) -> True
    (_, "") -> True
    _       -> False

getOperand c = let (x, y) = splitAt 2 c in
    case break (\x -> x `elem` "+-*/") y of
        (a, "") -> (x ++ a, "", "")
        (a, b ) -> (x ++ a, tail b, head b : [])


toTAC code = toTAC' code [] (Map.fromList []) (Map.fromList []) where
    toTAC' (c:cs) res m t
        | isCommd "movl" c = let x = trans c "=" m in toTAC' cs (fst x:res) (snd x) (updateT c 4 t)
        | isCommd "movb" c = let x = trans c "=" m in toTAC' cs (fst x:res) (snd x) (updateT c 1 t)
        | isCommd "mov"  c = let x = trans c "=" m in toTAC' cs (fst x:res) (snd x) t
        | isCommd "add"  c = let x = trans c "+" m in toTAC' cs (fst x:res) (snd x) t
        | isCommd "sub"  c = let x = trans c "-" m in toTAC' cs (fst x:res) (snd x) t
        | isCommd "imul" c = let x = trans c "*" m in toTAC' cs (fst x:res) (snd x) t
        | isCommd "idiv" c = let x = trans c "/" m in toTAC' cs (fst x:res) (snd x) t
        | isCommd "push" c = let x = trans c "c" m in toTAC' cs (fst x:res) (snd x) t
        | isCommd "set"  c = let x = trans c "c" m in toTAC' cs (fst x:res) (snd x) t
        | isCommd "cmp"  c = let x = trans c "~" m in toTAC' cs (fst x:res) (snd x) t
        | isCommd "leaq" c = let x = trans c "l" m in toTAC' cs (fst x:res) (snd x) t
        | isCommd "j"    c = let x = trans c "j" m in toTAC' cs (fst x:res) (snd x) t
        | isCommd "pop"  c =
            let
                (tgt, res') = findNearestPush res
                tgt' = if head tgt == '*' then tail tgt else tgt
                c' = head $ conn_inst "movl" tgt' (getCommdTarget c)
                x  = trans c' "=" m
            in toTAC' cs (fst x:res') (snd x) t
        | head c /= '\t'   = toTAC' cs ((c, ""):res) m t
        | otherwise        = let x = trans c "" m in toTAC' cs (fst x:res) (snd x) t
    toTAC' [] res _ t = (reverse res, t)

    updateT c n t = let (_, tgt) = getOperand c in
        if "%rbp" `isInfixOf` tgt
        then let x = takeWhile (\x -> x /= '(') tgt in Map.insert x n t
        else t

    getOperand c = (a, b) where
        (a, b) = case splitOn ", " $ last $ splitOn "\t" c of
            (x:y:_) -> (x, y)
            (x:[]) -> (x, "")
            _ -> ("", "")

    isCommd c c' = ("\t" ++ c) `isPrefixOf` c' || c `isPrefixOf` c'

    getCommd = head . tail . splitOn "\t"
    getCommdTarget = last . splitOn "\t"

    findNearestPush res = let (a, b) = break (isCommd "push" . fst) res in
        (snd $ head b, a ++ tail b)

    trans c op m = let (a, b) = getOperand c in case op of
        ""  -> ((c, ""), m)
        "c" -> let (cmd, tgt) = (getCommd c, getCommdTarget c) in
            ((cmd, fixl tgt), m)
        "=" -> let r = fixr b in ((fst r, fixl a), snd r)
        "l" -> let r = fixr b in ((fst r, tail $ fixRegG a), snd r)
        "/" -> let r = fixr b ; b = "%eax" in
            ((fst r, fixl b ++ op ++ fixl a), snd r)
        "~" -> (("cmp", fixl a ++ op ++ fixl b), m)
        "j" -> ((getCommd c, getCommdTarget c), m)
        _   -> let r = fixr b in
            ((fst r, fixl b ++ op ++ fixl a), snd r)
        where
            fixRegG rg = let x = getGroupVal rg in
                if isRegGroup rg
                then head x ++ "(" ++ intercalate "," (map fixl $ tail x) ++ ")"
                else rg

            fixReg r = if r `elem` concat registers
                then let i = get_reg_index r in
                    i !! 1
                else r
            fixl xi = let x = fixRegG $ fixReg xi in
                if '%' `notElem` x || (isDigit $ last x) || isRegGroup x
                then if isRegGroup x then "*" ++ x else x
                else case Map.lookup x m of
                    Nothing -> x ++ "0"
                    Just  i -> x ++ show i
            fixr xi = let x = fixRegG $ fixReg xi in
                if '%' `notElem` x || isRegGroup x
                then (if isRegGroup x then "*" ++ x else x, m)
                else case Map.lookup x m of
                    Nothing -> (x ++ "1", Map.insert x 1 m)
                    Just  i -> (x ++ show (i + 1), Map.insert x (i + 1) m)


