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


globalOptimizeOnAFunction bbs =
    let
        (tac, size) = unzip $ map (toTAC . getCode) bbs
        id = map getId bbs
        entry = map getEntry bbs
        f inp = map (\x -> (\(a, _, _) -> a) $ commonSubexprElim x Map.empty Map.empty)
            $ globalDeadCodeElim inp id entry
    in
        -- untilNoChange f tac
        globalConstCopyPropagation tac id entry
        -- globalDeadCodeElim tac id entry


globalDeadCodeElim tacs ids entries = untilNoChange (\x -> globalDeadCodeElimOnce x ids entries) tacs

globalDeadCodeElimOnce tacs ids entries =
    let
        id_liv = Map.insert (last ids) ["%eax"]
            $ Map.fromList
            $ zip ids
            $ replicate (length ids) []
        liv_final = untilNoChange (buildLiveness (head tacs) (head ids) (head entries) []) id_liv
    in
        (\id -> let tac = getItem id id_tac
                    liv = tail $ collectLivness tac (getItem id liv_final)
                in  doElimination tac liv) `map` ids
    where
        id_entry_t = zip ids entries
        id_entry   = Map.fromList id_entry_t
        id_tac     = Map.fromList $ zip ids tacs

        doElimination tac liv = elim tac liv [] where
            elim (x@(a, b):tr) (l:lr) z = if not $ isReg a then x : (elim tr lr z) else
                if isLiv a l then x : (elim tr lr z) else elim tr lr z
            elim [x] _ z = x:z
            elim _ _ z = z

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
                push_up  = map rmRegIndex $ head liv
                upstream = find_upstream id
                id_liv'  = foldr (\id m -> updateLiv' id push_up m) id_liv upstream
            in  id_liv'
            where
                updateLiv' id l id_liv =
                    let x = l ++ getItem id id_liv
                    in Map.insert id (rmDupItem x) id_liv

        find_upstream id = map fst $ filter (\x -> id `elem` snd x) id_entry_t

        isLiv r liv = let fixed_r = rmRegIndex r in
            r `elem` liv || fixed_r `elem` liv

        collectLivness tac liv = init $ foldr step [liv] tac where
            step x z = (getLivness x (head z)) : z

            getLivness (a, b) init = getRegs b ++ specialRemove a init

            specialRemove a l = let fixed_reg = rmRegIndex a in
                removeWhere (\x -> x == fixed_reg || x == a) l


globalConstCopyPropagation tacs ids entries =
    let
        id_const_init = Map.fromList $ zip ids (replicate (length ids) [])
        -- id_const :: Map.Map Int [(String, String)]
        id_const = findConst (head tacs) (head ids) (head entries) [] id_const_init
    in
        -- FIXME -4(%rbp1)1 passed but -4(%rbp0)1 needed
        map (\id -> let tac = getItem id id_tac
                        eq  = Map.fromList $ getItem id id_const
                    in  (\(a, _, _) -> a) $ commonSubexprElim tac eq eq) ids
    where
        id_entry = Map.fromList $ zip ids entries
        id_tac   = Map.fromList $ zip ids tacs

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
                    Nothing -> (fst c, doCopy (snd c) eq)
                    Just  x -> (fst c, "*" ++ x)
                -- copy propagation
                else (fst c, doCopy (snd c) eq)
            Just  x -> (fst c, x)

    doCopy c eq = case getOperand c of
        (a, "", _) -> case copy a eq of
            Nothing -> if isRegGroup a
                then let (h:val) = getGroupVal a in h ++ copyIntoG val [] ++ (snd $ break (==')') a)
                else a
            Just  x -> x
        (a, b, op) -> doCopy a eq ++ op ++ doCopy b eq
        where
            copyIntoG (c:cs) z = case copy c eq of
                Nothing -> copyIntoG cs (c:z)
                Just  x -> if isRegGroup x then copyIntoG cs (c:z) else copyIntoG cs (x:z)
            copyIntoG [] z = "(" ++ intercalate "," (reverse z) ++ ")"

            copy a eq = if a == "" || "%rsp" `isPrefixOf` a then Nothing else
                case Map.lookup a eq of
                    Nothing -> if head a == '*'
                        then case Map.lookup (tail a) eq of
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


toTAC code = toTAC' code [] Map.empty Map.empty where
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
        "~" -> (("cmp", fixl b ++ op ++ fixl a), m)
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
                if isReg x
                then let idx = getIdx x m in
                    (if isRegGroup x then "*" else "") ++ x ++ show idx
                else x
            fixr xi = let x = fixRegG $ fixReg xi in
                if isReg x
                then let idx = 1 + getIdx x m in
                    ((if isRegGroup x then "*" else "") ++ x ++ show idx, Map.insert x idx m)
                else (x, m)

            getIdx x m = case Map.lookup x m of
                Nothing -> 0
                Just  i -> i
