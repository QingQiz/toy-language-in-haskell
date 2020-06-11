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
        (++) (getHeader cfg) $ concat $ map globalOptimizeOnAFunction fs
        -- map globalOptimizeOnAFunction fs
    where
        splitWithFunction bbs = foldr step [] bbs where
            step b z@(x:xs) = case getEntry b of
                [] -> [b]:z
                _  -> (b:x):xs
            step b [] = [[b]]


globalOptimizeOnAFunction bbs =
    let
        tacs          = map (toTAC . getCode) bbs
        ids           = map getId bbs
        entries       = map getEntry bbs
        optimized_tac = untilNoChange (\x -> optimizeOnce x ids entries) tacs
    in
        -- error $ show tacs
        fromTAC optimized_tac ids entries
    where
        optimizeOnce tacs ids entries =
            let dead_code = globalDeadCodeElim tacs ids entries
                expr_elim = map (\x -> fst' $ commonSubexprElim x Map.empty Map.empty) dead_code
                copy_prop = globalConstCopyPropagation expr_elim ids entries
            in  copy_prop


globalDeadCodeElim tacs ids entries = untilNoChange (\x -> globalDeadCodeElimOnce x ids entries) tacs

globalDeadCodeElimOnce [] _ _ = []
globalDeadCodeElimOnce tacs ids entries =
    let
        liv_final = livnessAnalysis tacs ids entries
    in
        (\id -> let tac = getItem id id_tac
                    liv = tail $ collectLivness tac (getItem id liv_final)
                in  doElimination tac liv) `map` ids
    where
        id_tac = Map.fromList $ zip ids tacs
        doElimination tac liv = elim tac liv where
            elim (x@(a, b):tr) (l:lr)
                | isReg a = if isLiv a l then x : (elim tr lr) else elim tr lr
                | a == "cltd" || a == "cltq" = if isLiv' "%eax" l then x : (elim tr lr) else elim tr lr
                | otherwise = x : (elim tr lr)
            elim _ _ = []

        isLiv r liv  = let fixed_r = rmRegIndex r in
            r `elem` liv || fixed_r `elem` liv
        isLiv' r liv = r `elem` (map rmRegIndex liv)


livnessAnalysis tacs ids entries =
    let
        g_vars = filter (\x -> "rip" `isInfixOf` x)
            $ concat
            $ map (\(a, b) -> getRegs a ++ getRegs b)
            $ concat tacs
        id_liv = (\inp -> Map.insert (last ids) (getItem (last ids) inp ++ ["%eax", "%rsp", "%rbp"] ++ g_vars) inp)
            $ Map.fromList
            $ zipWith findGV ids
            $ replicate (length ids) []

        findGV id _ = (id, findGVar (getItem id id_tac)) where
            findGVar ((a, b):ts)
                | "ebx" `isInfixOf` a && "rip" `isInfixOf` b && head b /= '*' =
                      ("*(%ebx)") : findGVar ts
                | otherwise = findGVar ts
            findGVar _ = []
    in
        untilNoChange (buildLiveness (head tacs) (head ids) (head entries) []) id_liv
    where
        id_entry_t = zip ids entries
        id_entry   = Map.fromList id_entry_t
        id_tac     = Map.fromList $ zip ids tacs

        buildLiveness tac id [] vis id_liv = updateLiv tac id id_liv

        buildLiveness tac id entry vis id_liv =
            let (id_liv', _) = foldr step (id_liv, vis) entry where
                    step x z@(id_liv, vis) = if x `elem` vis then z else
                        let tac     = getItem x id_tac
                            entry   = getItem x id_entry
                            id_liv' = buildLiveness tac x entry (x:vis) id_liv
                        in (id_liv', x:vis)
            in updateLiv tac id id_liv'

        updateLiv tac id id_liv =
            let liv      = collectLivness tac $ getItem id id_liv
                push_up  = if null liv then [] else map rmRegIndex $ head liv
                upstream = find_upstream id
                id_liv'  = foldr (\id m -> updateLiv' id push_up m) id_liv upstream
            in  id_liv'
            where
                updateLiv' id l id_liv =
                    let x = l ++ getItem id id_liv in Map.insert id (rmDupItem x) id_liv

        find_upstream id = map fst $ filter (\x -> id `elem` snd x) id_entry_t


collectLivness tac liv = foldr step [liv] tac where
    step x z = (getLivness x (head z)) : z

    getLivness (a, b) init
        | head a == 'j'        = init
        | a == "cmp"           = getRegs b ++ init
        | "set" `isPrefixOf` a = specialRemove b init
        | a == "pushq"         = getRegs b ++ init
        | a == "call"          = let n =  (+) 1 $ read $ last $ splitOn "#" b
                                 in  "%eax" : (take n $ tail $ tail $ map (!!1) registers) ++ init
        | a == "cltd"          = "%eax" : init
        | a == "cltq"          = "%eax" : init
        | otherwise            = (if isRegGroup a then tail $ getGroupVal a else [])
                                 ++ (if head a == '*' then getRegs $ tail a else [])
                                 ++ getRegs b
                                 ++ specialRemove a init

    specialRemove a l = let fixed_reg = rmRegIndex a in
        removeWhere (\x -> x == fixed_reg || x == a) l


fixRegIndexInLiv tac liv =
    let m_init = Map.fromList $ zip regs [0,0..]
        regs   = (["%rsp", "%rbp"] ++ map (!!1) registers)
    in  fixLiv' (("",""):tac) liv m_init
    where
        fixLiv' (t:tac) (l:liv) m =
            let (l', m') = foldr step ([], m) (rmDupItem l)
            in l' : fixLiv' tac liv m'
            where step x (res, m) =
                    let (r', m') = fixRegIndex t x m
                    in  (r':res, m')
        fixLiv' [] [] _ = []

        getItem' x m = case Map.lookup x m of {Nothing -> 0; Just x -> x}

        fixRegIndex (a, b) r m =
            let regs = getRegs a ++ getRegs b
                m'   = foldr (\x z -> updateM x z) m regs
            in  if getRegIndex r == ""
                then (r ++ (show $ getItem' r m'), m')
                else (r, m')

        updateM r m = let r' = rmRegIndex r
                          idx_new = getRegIndex r
                          idx_now = getItem' r' m
                      in  if (read idx_new) > idx_now
                          then Map.insert r' (read idx_new) m
                          else m


globalConstCopyPropagation [] _ _ = []
globalConstCopyPropagation tacs ids entries =
    let
        id_const_init  = Map.fromList $ zip ids (replicate (length ids) [])
        -- id_const :: Map.Map Int [(String, String)]
        id_const_final = untilNoChange (findConst (head tacs) (head ids) (head entries) []) id_const_init
        id_const = Map.fromList
            $ map (\(a, b) -> (,) a $ map (\(l, r) -> (rmRegIndex l, rmRegIndex r)) b)
            $ Map.toList id_const_final
    in
        map (\id -> let tac = getItem id id_tac
                        eq  = Map.fromList $ getItem id id_const
                    in  doCopyPropagation tac eq) ids
    where
        id_entry = Map.fromList $ zip ids entries
        id_tac   = Map.fromList $ zip ids tacs

        doCopyPropagation tac eq = snd $ foldr step (eq, []) tac where
            step (l, r) (eq, res) =
                let fixed_l = rmRegIndex l
                in  case Map.lookup fixed_l eq of
                    Nothing -> (eq,  (l, doCopy r eq True):res)
                    Just  _ -> (Map.delete fixed_l eq, (l, doCopy r eq True):res)

        findConst tac id [] _ const = updateConst id tac const

        findConst tac id entry vis const =
            let (const', _) = foldr step (const, vis) entry where
                    step x z@(const, vis) = if x `elem` vis then z else
                        let tac = getItem id id_tac
                            entry = getItem id id_entry
                            const' = findConst tac x entry (x:vis) const
                        in  (const', x:vis)
            in updateConst id tac const

        updateConst id tac const =
            let (tac', _, eq) = commonSubexprElim tac Map.empty Map.empty
                eq' = Map.toList $ Map.fromList $ filter (isConst . snd) $ Map.toList eq
            in  foldr update const eq'
            where
                update (l, r) z = Map.fromList $ foldr step [] (Map.toList z) where
                    step x@(id, const_l) z = let c = break ((==l) . fst) const_l
                        in  case snd c of
                            [] -> (id, (l, r):const_l):z
                            (a, b):rst -> if b == r then x:z else (id, (++) (fst c) $ (a, ""):rst):z


commonSubexprElim [] _ _ = ([], Map.empty, Map.empty)
commonSubexprElim tac init_z init_eq = untilNoChange convert (tac, Map.empty, Map.empty) where
    convert (tac, _, _) = csElim tac [] init_z init_eq

    csElim (c:cs) code z eq =
        csElim cs (doReplace c z eq : code) (updateZ c z) (updateEQ c eq)
    csElim [] code z eq = (constFolding $ reverse code, z, eq)

    updateZ c z =
        if snd c == "" || isLetter (head $ fst c) || '%' `notElem` (snd c)
        then z
        else if not $ isSimple (snd c) then Map.insert (snd c) (fst c) z else z

    updateEQ c z =
        if snd c == "" || '%' `notElem` fst c || isLetter (head $ snd c) || "%rbp" `isPrefixOf` (fst c)
        then z
        else if isSimple (snd c) then Map.insert (fst c) (snd c) z else z

    doReplace x@(_, "") _ _ = x
    doReplace c z eq =
        -- common subexpression elimination
        case Map.lookup (snd c) z of
            Nothing -> if isRegGroup (snd c) && head (snd c) == '*'
                then case Map.lookup (tail (snd c)) z of
                    -- copy propagation
                    Nothing -> (fst c, doCopy (snd c) eq False)
                    Just  x -> (fst c, "*" ++ x)
                -- copy propagation
                else (fst c, doCopy (snd c) eq False)
            Just  x -> (fst c, x)


doCopy c eq ignoreIdx = case getOperand c of
    (a, "", _) -> case copy a eq of
        Nothing -> if isRegGroup a
            then let (h:val) = getGroupVal a in h ++ copyIntoG val [] ++ (tail $ snd $ break (==')') a)
            else a
        Just  x -> x
    (a, b, op) -> doCopy a eq ignoreIdx ++ op ++ doCopy b eq ignoreIdx
    where
        copyIntoG (c:cs) z = case copy c eq of
            Nothing -> copyIntoG cs (c:z)
            Just  x -> if isRegGroup x then copyIntoG cs (c:z) else copyIntoG cs (x:z)
        copyIntoG [] z = "(" ++ intercalate "," (reverse z) ++ ")"

        copy a eq = let a' = if ignoreIdx then rmRegIndex a else a in
            if a' == "" || "%rsp" `isPrefixOf` a'
            then Nothing
            else case Map.lookup a' eq of
                Nothing -> if head a' == '*'
                    then case Map.lookup (tail a') eq of
                        Nothing -> Nothing
                        Just  x -> Just $ '*':x
                    else Nothing
                Just  x -> Just x


constFolding tac = untilNoChange foldOnce tac where
    foldOnce tac =
        let bk = break (\x -> 2 == (cnt '$' $ snd x )) tac
            (h, (a, b):r) = bk -- this won't be evaluated when (snd bk) is empty
            (x, y, op) = getOperand (replace "$" "" b)
            (x', y') = (read x :: Int, read y :: Int)
            res x = (++) h $ (a, '$' : show x) : r
            resj x l = if x then (++) h $ ("jmp", l) : (tail r) else h ++ (tail r)
        in if null $ snd bk then tac else case op of
            "+" -> res $ x' + y'
            "-" -> res $ x' - y'
            "*" -> res $ x' * y'
            "/" -> res $ x' `div` y'
            "~" -> let (j, l) = head r in case j of
                "je"  -> (x' == y') `resj` l
                "jne" -> (x' /= y') `resj` l
                "jg"  -> (x' >  y') `resj` l
                "jl"  -> (x' <  y') `resj` l
                "jge" -> (x' >= y') `resj` l
                "jle" -> (x' <= y') `resj` l
                "jmp" -> True       `resj` l
                _ -> error $ show (j, l)


toTAC code = toTAC' code [] Map.empty where
    toTAC' (c:cs) res m
        | isCommd "mov"  c = f "="
        | isCommd "add"  c = f "+"
        | isCommd "sub"  c = f "-"
        | isCommd "imul" c = f "*"
        | isCommd "idiv" c = f "/"
        | isCommd "push" c = f "c"
        | isCommd "set"  c = f "c"
        | isCommd "call" c = f "c"
        | isCommd "cmp"  c = f "~"
        | isCommd "leaq" c = f "l"
        | isCommd "j"    c = f "j"
        | isCommd "pop"  c =
            let (tgt, res') = findNearestPush res
                c' = head $ conn_inst "movl" tgt (getCommdTarget c)
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
            "c" -> let (cmd, tgt) = (getCommd c, getCommdTarget c) in
                ((cmd, fixl tgt), m)
            "=" -> let r = fixr b in ((fst r, fixl a), snd r)
            "l" -> let r = fixr b in ((fst r, tail $ fixl a), snd r)
            "/" -> let r = fixr b ; b = "%eax" in
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

            fixReg r | r `elem` concat registers = let i = get_reg_index r in i !! 1
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
        (liv', tac', params) = unpack $ zipWith3 fix ids liv tacs where
            unpack x = let (a, b) = unzip x
                           a'     = concat $ map tail a
                           a''    = head $ map rmDupItem $ fixLocal $ concat a
                       in  (a'' : (map rmDupItem $ fixLocal a'), concat b, first a)

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
                    findReg (i:is) = let (a, _) = tac' !! i in a : findReg is
                    findReg []     = []
                ret_val   = findReg $ findIdx tac' 0 where
                    findIdx ((a, b):t) n
                        | "j" `isPrefixOf` a && "END" `isSuffixOf` b = (:) (n - 1) $ findIdx t (n + 1)
                        | a == "leave"                               = (:) (n - 1) $ findIdx t (n + 1)
                        | a == "ret"                                 = (:) (n - 1) $ findIdx t (n + 1)
                        | otherwise                                  = findIdx t (n + 1)
                    findIdx [] _ = []
                    findReg (i:is) = let (a, _) = tac' !! i in
                        if "eax" `isInfixOf` a then a : findReg is else findReg is
                    findReg [] = []
                div       = findReg $ findIdx tac' 0 where
                    findIdx ((a, b):t) n
                        | a == "cltd" = (:) (n - 1) $ findIdx t (n + 1)
                        | otherwise   = findIdx t (n + 1)
                    findIdx [] _ = []
                    findReg (i:is) = let (a, _) = tac' !! i in
                        if "eax" `isInfixOf` a then a : findReg is else findReg is
                    findReg [] = []
                g_vars    = filter (\x -> "rip" `isInfixOf` x)
                            $ concat
                            $ map (\(a, b) -> getRegs a ++ getRegs b) tac'
                final     = params ++ func_call ++ ret_val ++ g_vars
            in  Map.fromList $ zip final $ map rmRegIndex final
    in
        (func tac' liv' (tail liv') alloc_init [] [] [])
    where
        id_entry = Map.fromList $ zip ids entries
        id_tac   = Map.fromList $ zip ids tacs
        reg_all  = map (!!1) registers

        func (t:tac) (l_up:liv_up) (l_down:liv_down) alloc tac' spin spout =
            let (t', alloc', spin', spout') = allocaAStep t l_up l_down alloc
            in  func tac liv_up liv_down alloc' (t':tac') (spin' : spin) (spout' : spout)
        func _ _ _ _ tac spin spout = (reverse tac, reverse spin, reverse spout)

        allocaAStep t@(tl, tr) l_up l_down alloc =
            let
                regl = getRegs tl
                regr = getRegs tr
                -- (t' , a' , si , so ) = allocaAStep' t  (filter (\x -> x `elem` regl) l_down) l_up   alloc
                -- (t'', a'', si', so') = allocaAStep' t' (filter (\x -> x `elem` regr) l_down) l_down a'
            in
                allocaAStep' t l_down l_down alloc
                --- XXX return a * 5 + b -> mul 5 a, add a b, ret b
                ---     current             mov a b, mul 5 b, add b c, ret c
                -- (t'', a'', si ++ si', so ++ so')

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
                tryAlloc r alloc  =
                    let valid     = filter (\(a, b) -> a `elem` l) $ Map.toList alloc
                        reg_using = map snd valid
                        regs      = filter (\x -> x `notElem` reg_using) reg_all
                    in  case Map.lookup r alloc of
                            Nothing
                                | isRegGroup r && not ("rbp" `isInfixOf` r) ->
                                      let (_:v:[]) = getGroupVal r
                                      in  tryAlloc v alloc
                                | otherwise -> doAlloca r regs alloc
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
                    in  foldr step (t, alloc, [], []) regs
                    where
                        step reg (t, alloc, spin, spout) =
                            let (t', alloc', spin', spout') = doReplace t reg
                                spin''                      = mergeInto spin' spin
                                spout''                     = mergeInto spout' spout
                            in  (t', alloc', spin'', spout'')

                        -- return: (tac', alloc', maybe spill in, maybe spill out)
                        doReplace t@(a, b) reg = case Map.lookup reg alloc of
                            Nothing -> (t, alloc, Nothing, Nothing)
                            Just  x | "spill" `isInfixOf` x ->
                                          let (alloc', spin@(_, reg'), spout) = spillIn reg alloc
                                              tac' = (replace reg reg' a, replace reg reg' b)
                                          in  (tac', alloc', Just spin, spout)
                                    | otherwise ->
                                          let tac' = (replace reg x a, replace reg x b)
                                          in  (tac', alloc, Nothing, Nothing)


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
                    findFirst' r _ _ = error $ "can not find " ++ r ++ " in" ++ show liv

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
        -- tacs
        doTrans tac'
        -- registerRealloca tacs ids entries
    where
        doTrans (t:ts) = trans t ++ doTrans ts
        doTrans _ = []

        trans t@(a, b)
            | isLabel a = a : []
            | isCmd a && null b = conn1 a : []
            | isCmd a = case getOperand b of
                  (x, [], []) -> conn2 a b : []
                  (x, y, o)   -> conn3 (getCmd o) x y : []
            | otherwise = case getOperand b of
                  (x, [], []) | head x == '*' -> conn3 "movl" (tail x) a : []
                              | head a == '*' -> conn3 "movl" x (tail a) : []
                              | isRegGroup x || isRegGroup a -> conn3 "leaq" x a : []
                              | otherwise     -> conn3 "movl" x a : []
                  (x, y, o) | o == "/"  -> conn2 "idivl" y : []
                            | x == a    -> conn3 (getCmd o) y a : []
                            | y == a && (o == "*" || o == "+") -> conn3 (getCmd o) x a : []
                            | otherwise -> conn3 "movl" x a : conn3 (getCmd o) y a : []

        getCmd op = case op of
            "+" -> "addl"
            "-" -> "subl"
            "*" -> "imull"
            "/" -> "idivl"
            "~" -> "cmpl"
        isCmd = not . isReg
        isLabel = elem ':'

        conn1 a     = "\t" ++ a
        conn2 a b   = "\t" ++ a ++ "\t" ++ b
        conn3 a b c = "\t" ++ a ++ "\t" ++ b ++ ", " ++ c
