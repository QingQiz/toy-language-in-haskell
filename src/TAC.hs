module TAC where

import Register
import Functions
import PeepHole
import RegisterAlloca

import Data.Char
import Data.List
import Data.List.Split
import Data.List.Utils
import qualified Data.Map as Map


toTAC code = toTAC' code [] Map.empty where
    toTAC' (c:cs) res m
        | isCommd "mov"  c = f "="
        | isCommd "add"  c = f "+"
        | isCommd "sub"  c = f "-"
        | isCommd "imul" c = f "*"
        | isCommd "idiv" c = f "/"
        | isCommd "push" c = f "c"
        | isCommd "set"  c = f "c"
        | isCommd "call" c = f "f"
        | isCommd "cmp"  c = f "~"
        | isCommd "leaq" c = f "l"
        | isCommd "j"    c = f "j"
        | isCommd "test" c = f "t"
        | isCommd "neg"  c = f "n"
        | isCommd "pop"  c =
            let (tgt, res') = findNearestPush res
                c' = head $ conn_inst "movq" tgt (getCommdTarget c)
                x  = (\((a , b), c) -> ((a, tgt), c)) $ trans c' "=" m
            in toTAC' cs (fst x:res') (snd x)
        | head c /= '\t'  = toTAC' cs ((c, ""):res) m
        | otherwise       = let x = trans (dropWhile (=='\t') c) "" m in toTAC' cs (fst x:res) (snd x)
        where f o = let x = trans c o m in toTAC' cs (fst x:res) (snd x)
    toTAC' [] res _ = reverse res

    getOperand c = (a, b) where
        (a, b) = case splitOn ", " $ last $ splitOn "\t" c of
            (x:y:_) -> (x, y)
            (x:[]) -> (x, "")
            _ -> ("", "")

    findNearestPush res = let (a, b) = break (isCommd "push" . fst) res in
        (snd $ head b, a ++ tail b)

    trans c op m =
        let (a, b) = getOperand c
        in  case op of
            ""  -> ((c, ""), m)
            "f" -> let (a, b) = trans c "c" m
                       idx    = 1 + getIdx "%rax" m
                   in  (a, Map.insert "%rax" idx b)
            "c" -> let (cmd, tgt) = (getCommd c, getCommdTarget c) in
                ((cmd, fixl tgt), m)
            "t" -> (("test", fixl a), m)
            "n" -> trans (head $ conn_inst "imulq" "$-1" a) "*" m
            "=" -> let r = fixr b in ((fst r, fixl a), snd r)
            "l" -> let r = fixr b in ((fst r, fixAddr $ fixl a), snd r)
            "/" -> let r = fixr b ; b = "%rax" in
                ((fst r, fixl b ++ op ++ fixl a), snd r)
            "~" -> (("cmp", fixl b ++ op ++ fixl a), m)
            "j" -> ((getCommd c, getCommdTarget c), m)
            _   -> let r = fixr b in
                ((fst r, fixl b ++ op ++ fixl a), snd r)
        where
            fixRegG rg = let (h:v) = getGroupVal rg in
                if isRegGroup rg
                then h ++ "(" ++ intercalate "," (map fixl v) ++ ")"
                else rg

            fixReg r | r `elem` concat registers =
                           let i = (get_reg_index r) !! 0
                           in  if isDigit $ last i
                               then i ++ "x"
                               else i
                     | otherwise = r
            fixl xi = let x = fixRegG $ fixReg xi in
                if isReg x
                then let idx = getIdx x m
                     in  (if isRegGroup x then "*" else "") ++ x ++ show idx
                else x
            fixr xi = let x = fixRegG $ fixReg xi in
                if isReg x
                then let idx = 1 + getIdx x m in
                    ((if isRegGroup x then "*" else "") ++ x ++ show idx, Map.insert x idx m)
                else (x, m)

            getIdx x m = case Map.lookup x m of
                Nothing -> 0
                Just  i -> i


fromTAC tacs ids entries =
    let
        (tac', spin, spout) = registerRealloca tacs ids entries
    in
        reduceUselessCmp $ reduceUselessJmp $ reduceStackChange $ doTrans tac'
    where
        doTrans (t:ts) = trans t ++ doTrans ts
        doTrans _ = []

        trans t@(a, b)
            | isLabel a = a : []
            | isCmd a && null b = conn1 a : []
            | isCmd a = case getOperand b of
                  (x, [], []) | a == "test"   -> [conn3 "testq" x x]
                              | otherwise     -> [conn2 a b]
                  (x, y, o)   -> [conn3 (getCmd o) y x]
            | otherwise = case getOperand b of
                  (x, [], []) | x == a -> []
                              | isRegGroup x && isRegGroup a ->
                                    let reg = last $ getGroupVal x
                                    in  [conn2 "pushq" reg] ++ trans (reg, x) ++ trans (a, reg) ++ [conn2 "popq" reg]
                              | head x == '*' -> [conn3 "movq" x a]
                              | head x /= '*' && isRegGroup x  -> [conn3 "leaq" x a]
                              | otherwise -> [conn3 "movq" x a]

                  (x, y, o) | o == "+" && x == "$0" -> trans (a, y)
                            | o == "+" && y == "$0" -> trans (a, x)
                            | o == "-" && y == "$0" -> trans (a, x)
                            | o == "*" && (x == "$0" || y == "$0") -> trans (a, "$0")
                            | o == "*" && x == "$1"  -> trans (a, y)
                            | o == "*" && y == "$1"  -> trans (a, x)
                            | o == "*" && x == "$-1" && y == a -> [conn2 "negq" y]
                            | o == "*" && x == "$-1" && y /= a -> [conn3 "movq" y a, conn2 "negq" a]
                            | o == "*" && y == "$-1" && x == a -> [conn2 "negq" x]
                            | o == "*" && y == "$-1" && x /= a -> [conn3 "movq" x a, conn2 "negq" a]
                            | o == "/" && x == "$0"  -> trans (a, "$0")
                            | o == "/" && y == "$1"  -> trans (a, x)
                            | o == "/" && x == "%rax" -> [conn1 "cqto", conn2 "idivq" y]
                            | o == "/" && x /= "%rax" -> [conn3 "movq" x "%rax", conn1 "cqto", conn2 "idivq" y]
                            | x == a -> [conn3 (getCmd o) y a]
                            | y == a && (o == "*" || o == "+") -> [conn3 (getCmd o) x a]
                            | otherwise -> trans (a, x) ++ trans (a, a ++ o ++ y)

        getCmd op = case op of
            "+" -> "addq"
            "-" -> "subq"
            "*" -> "imulq"
            "/" -> "idivq"
            "~" -> "cmpq"
        isCmd = not . isReg
        isLabel = elem ':'

        fixHead = fixAddr
        conn1 a     = "\t" ++ fixHead a
        conn2 a b   = "\t" ++ fixHead a ++ "\t" ++ fixHead b
        conn3 a b c = "\t" ++ fixHead a ++ "\t" ++ fixHead b ++ ", " ++ fixHead c
