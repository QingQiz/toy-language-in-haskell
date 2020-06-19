module TAC where

import Debug.Trace
import Livness
import Register
import Functions
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

    isCommd c c' = ("\t" ++ c) `isPrefixOf` c' || c `isPrefixOf` c'

    getCommd = head . tail . splitOn "\t"
    getCommdTarget = last . splitOn "\t"

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
            "=" -> let r = fixr b in ((fst r, fixl a), snd r)
            "l" -> let r = fixr b in ((fst r, tail $ fixl a), snd r)
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


registerRealloca tacs ids entries =
    let
        id_liv_final = livnessAnalysis tacs ids entries
        liv          = (\id -> let tac = getItem id id_tac
                                   liv = collectLivness tac (getItem id id_liv_final)
                               in  fixRegIndexInLiv tac liv) `map` ids
        (liv', tac', params, static) = unpack $ zipWith3 fix ids liv tacs where
            unpack x = let (a, b) = unzip x
                           a'     = concat $ map tail a
                           a''    = head $ map rmDupItem $ fixLocal $ concat a
                           static = concat $ foldr (\x z -> if null x then z else head x:z) [] a
                       in  (a'' : (map rmDupItem $ fixLocal a'), concat b, first a, filter (\x -> (not . isInfixOf "rbp") x) static)

            first (l:ls) = if null l then first ls else head l

            fix id l t = (map (map doFix) l, map (\(a, b) -> (doFix a, doFix b)) t)
                where
                    f x = "rsp" `isInfixOf` x || "rbp" `isInfixOf` x || "rip" `isInfixOf` x || isConst x

                    doFix r | not $ isReg r = r
                            | isRegGroup  r = let (h:vals) = getGroupVal r in
                                                  h ++ "(" ++ intercalate "," (map doFix vals) ++ ")"
                            | isSimple r    = if f r
                                              then rmRegIndex r
                                              else rmRegIndex r ++ show id ++ "0" ++ getRegIndex r
                            | otherwise     = let (a, b, op) = getOperand r in doFix a ++ op ++ doFix b
        alloc_init =
            let func_call = findReg $ findIdx tac' 0 where
                    findIdx ((a, b):t) n
                        | a == "call" = let x = read $ last $ splitOn "#" b
                                        in  map (n-) [1..(x+1)] ++ findIdx t (n + 1)
                        | otherwise   = findIdx t (n + 1)
                    findIdx [] _   = []
                    findReg (i:is) | i >= 0 = let (a, _) = tac' !! i in a : findReg is
                    findReg (i:is) | otherwise = findReg is
                    findReg []     = []
                ret_val   = findReg $ findIdx tac' 0 where
                    findIdx ((a, b):t) n
                        | "j" `isPrefixOf` a && "END" `isSuffixOf` b = (:) (n - 1) $ findIdx t (n + 1)
                        | a == "leave"                               = (:) (n - 1) $ findIdx t (n + 1)
                        | a == "ret"                                 = (:) (n - 1) $ findIdx t (n + 1)
                        | otherwise                                  = findIdx t (n + 1)
                    findIdx [] _ = []
                    findReg (i:is) | i >= 0 = let (a, _) = tac' !! i in
                        if "rax" `isInfixOf` a then a : findReg is else findReg is
                    findReg (i:is) | otherwise = findReg is
                    findReg [] = []
                div       = findReg $ findIdx tac' 0 where
                    findIdx ((a, b):t) n
                        | a == "cltd" = (:) (n - 1) $ findIdx t (n + 1)
                        | otherwise   = findIdx t (n + 1)
                    findIdx [] _ = []
                    findReg (i:is) | i >= 0 = let (a, _) = tac' !! i in
                        if "rax" `isInfixOf` a then a : findReg is else findReg is
                    findReg (i:is) | otherwise = findReg is
                    findReg [] = []
                g_vars    = filter (\x -> "rip" `isInfixOf` x)
                            $ concat
                            $ map (\(a, b) -> getRegs a ++ getRegs b) tac'
                fc_ret    = filter (\x -> "`fc" `isInfixOf` x)
                            $ concat
                            $ map (\(a, b) -> getRegs a ++ getRegs b) tac'
                final     = params ++ func_call ++ ret_val ++ g_vars ++ static ++ fc_ret
            in  Map.fromList $ zip final $ map fixRegIndex final
            where fixRegIndex r = let r' = rmRegIndex r
                                  in  if isDigit $ last $ init r' then init r' else r'
    in
        trace(show alloc_init)(func tac' liv' (tail liv') alloc_init [] [] [])
    where
        id_entry = Map.fromList $ zip ids entries
        id_tac   = Map.fromList $ zip ids tacs
        reg_all  = map (!!0) registers

        func (t:tac) (l_up:liv_up) (l_down:liv_down) alloc tac' spin spout =
            let (t', alloc', spin', spout') = allocaAStep t l_up l_down alloc
            in  func tac liv_up liv_down alloc' ((reverse t') ++ tac') (spin' : spin) (spout' : spout)
        func _ _ _ _ tac spin spout = (reverse tac, reverse spin, reverse spout)

        allocaAStep t@(tl, tr) l_up l_down alloc =
            let regl = getRegs tl
                regr = getRegs tr
            in  allocaAStep' t (regl ++ regr) l_down alloc

        regFree l alloc =
            let valid     = filter (\(a, b) -> a `elem` l) $ Map.toList alloc
                reg_using = map snd valid
            in  filter (\x -> x `notElem` reg_using) reg_all

        allocaAStep' t rs l alloc =
            let
                (alloc', spout) = foldr allocOne (alloc, []) rs
                (t', alloc'', spin, spout') = replaceRegWihtAlloca t alloc'
            in
                (t', alloc'', spin, spout' ++ spout)
            where
                allocOne r (alloc, spout) =
                    let (alloc', spout') = tryAlloc r alloc
                    in  (alloc', mergeInto spout' spout)

                -- tryAlloc :: register -> alloc -> (alloc, register to spill out)
                tryAlloc r alloc  = case Map.lookup r alloc of
                    Nothing
                        | isRegGroup r && not ("rbp" `isInfixOf` r) ->
                              let (_:v:[]) = getGroupVal r
                              in  tryAlloc v alloc
                        | otherwise -> doAlloca r (reverse $ regFree l alloc) alloc
                    Just  x -> (alloc, Nothing)
                    where
                        doAlloca r regs alloc
                            | null regs =
                                  let reg_in_use = regh ++ regt
                                      (alloc', spout) = spillOut reg_in_use alloc
                                      (alloc'', _) = tryAlloc r alloc'
                                  in  (alloc'', Just spout)
                            | r `elem` regh = case filter (\x -> x `elem` regt) regs of
                                  []   -> deft
                                  ri   -> (Map.insert r (head ri  ) alloc, Nothing)
                            | otherwise = deft
                            where regh  = getRegs (fst t)
                                  regt  = getRegs (snd t)
                                  deft  = (Map.insert r (head regs) alloc, Nothing)

                -- return (alloc', spill_out)
                spillOut regs alloc =
                    let
                        (a, b) = head $ snd $ break (\(a, b) -> a `notElem` regs) $ Map.toList alloc
                        alloc' = case Map.lookup "spill" alloc of
                                     Nothing -> Map.insert "spill" "0" $ Map.insert a "spill@0" alloc
                                     Just  x -> let l  = splitOn "," x
                                                    l' = map read l
                                                    i  = show $ head $ snd $ break (\x -> x `notElem` l') [0..]
                                                    sp = intercalate "," (i:l)
                                                in  Map.insert "spill" sp $ Map.insert a ("spill@" ++ i) alloc
                    in (alloc', (a, getItem a alloc'))

                -- return: (alloc', spill_in, Maybe spill_out), spill_in :: ()
                spillIn r alloc =
                    let
                        spill_id         = last $ splitOn "@" $ getItem r alloc
                        spill'           = intercalate "," $ delete spill_id $ splitOn "," $ getItem "spill" alloc
                        alloc'           = Map.insert "spill" spill' $ Map.delete r alloc
                        (alloc'', spout) = tryAlloc r alloc'
                        spin             = (getItem r alloc, getItem r alloc'')
                    in  (alloc'', spin, spout)

                replaceRegWihtAlloca t@(a, b) alloc =
                    let regs = getRegs a ++ getRegs b
                    in  foldr step ([t], alloc, [], []) regs
                    where
                        step reg (t, alloc, spin, spout) =
                            let (t', alloc', spin', spout') = doReplace (last t) reg
                                spin''                      = mergeInto spin' spin
                                spout''                     = mergeInto spout' spout
                            in  (t', alloc', spin'', spout'')

                        -- return: (tac', alloc', maybe spill in, maybe spill out)
                        doReplace t@(a, b) reg = case Map.lookup reg alloc of
                            Nothing -> ([t], alloc, Nothing, Nothing)
                            Just  x | "spill" `isInfixOf` x ->
                                          let (alloc', spin@(_, reg'), spout) = spillIn reg alloc
                                              t' = (replace reg reg' a, replace reg reg' b)
                                          in  ([t'], alloc', Just spin, spout)
                                    | null same_name ->
                                          let t' = (replace reg x a, replace reg x b)
                                          in  ([t'], alloc, Nothing, Nothing)
                                    | otherwise ->
                                          let t'  = (replace reg x a, replace reg x b)
                                              free  = reverse $ regFree l alloc
                                              (so, sa) = head same_name -- (orign, allocaed)
                                          in  ([(head free, sa), t'], Map.insert so (head free) alloc, Nothing, Nothing)
                                    where same_name = filter (\(bef, aft) -> bef /= reg && aft == x && bef `elem` l) $ Map.toList alloc

        -- > fixLocal [["%eax1"], ["%eax2"], ["%eax1"]] == [["%eax1"], ["%eax1", "%eax2"], ["%eax1"]]
        fixLocal liv =
            let
                regs = removeWhere f $ rmDupItem $ concat liv where
                    f x = "rsp" `isInfixOf` x || "rbp" `isInfixOf` x
            in
                foldr step liv regs
            where
                findFirst r liv = findFirst' r liv 0 where
                    findFirst' r (l:ls) n = if r `elem` l then n else findFirst' r ls (n + 1)
                    findFirst' r _ _ = error $ "could not find " ++ r ++ " in" ++ show liv

                findLast  r liv = (length liv) - 1 - findFirst r (reverse liv)

                step x liv = let b = findFirst x liv
                                 e = findLast  x liv
                             in  if b /= e
                                 then let (l, m, r) = splitAt3 (b + 1) e liv in
                                     l ++ (map (x:) m) ++ r
                                 else liv

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
                  (x, [], []) -> conn2 a b : []
                  (x, y, o)   -> conn3 (getCmd o) y x : []
            | otherwise = case getOperand b of
                  (x, [], []) | x == a -> []
                              | head x == '*' -> conn3 "movq" (tail x) a : []
                              | head a == '*' -> conn3 "movq" x (tail a) : []
                              | isRegGroup x || isRegGroup a -> conn3 "leaq" x a : []
                              | otherwise     -> conn3 "movq" x a : []
                  (x, y, o) | o == "+" && x == "$0" -> trans (a, y)
                            | o == "+" && y == "$0" -> trans (a, x)
                            | o == "-" && y == "$0" -> trans (a, x)
                            | o == "*" && (x == "$0" || y == "$0") -> trans (a, "$0")
                            | o == "*" && x == "$1" -> trans (a, y)
                            | o == "*" && y == "$1" -> trans (a, x)
                            | o == "/" && x == "$0" -> trans (a, "$0")
                            | o == "/" && y == "$1" -> trans (a, x)
                            | o == "/"  -> conn2 "idivq" y : []
                            | x == a -> conn3 (getCmd o) y a : []
                            | y == a && (o == "*" || o == "+") -> conn3 (getCmd o) x a : []
                            | otherwise -> trans (a, x) ++ trans (a, a ++ o ++ y)

        getCmd op = case op of
            "+" -> "addq"
            "-" -> "subq"
            "*" -> "imulq"
            "/" -> "idivq"
            "~" -> "cmpq"
        isCmd = not . isReg
        isLabel = elem ':'

        conn1 a     = "\t" ++ a
        conn2 a b   = "\t" ++ a ++ "\t" ++ b
        conn3 a b c = "\t" ++ a ++ "\t" ++ b ++ ", " ++ c


reduceStackChange code
    | null ref = reduceAllStack code
    | null neg_ref = reduceSubRspAndLeave code
    | otherwise = code
    where ref     = filter (\x ->  "(%rbp" `isInfixOf` x
                               || ("push"  `isInfixOf` x && not ("\t%rbp" `isInfixOf` x))
                               ||  "call" `isInfixOf` x) code
          neg_ref = filter (\x -> "\t-"    `isInfixOf` x || " -"   `isInfixOf` x) ref

          reduceAllStack = reduce1 (\x -> "rsp" `isInfixOf` x || "rbp" `isInfixOf` x || "leave" `isInfixOf` x)
          reduceSubRspAndLeave = reduce1 (\x -> ("sub" `isInfixOf` x && "rsp" `isInfixOf` x))


reduceUselessJmp = reduce2 f where
    f c cn = head cn == '.' && "\tj" `isPrefixOf` c && init cn `isSuffixOf` c


reduceUselessCmp = reduce2 f where
    f c cn = ("\tcmp" `isPrefixOf` c || "\ttest" `isPrefixOf` c)
      && not ("\tj"  `isPrefixOf` cn || "\tset" `isPrefixOf` cn)


reduce2 _ [] = []
reduce2 f code = reduce code (tail code) where
    reduce (c:cs) (cn:cns) | f c cn = reduce cs cns
                           | otherwise = c : reduce cs cns
    reduce c [] = c

reduce1 f (c:code) | f c = reduce1 f code
                   | otherwise = c : reduce1 f code
reduce1 _ _ = []
