module Optimizer where

import TAC
import CFG
import Livness
import Functions
import Data.Char
import Data.List
import Data.List.Utils
import qualified Data.Map as Map


-- doLocalOptimize :: CFG -> CFG
doGlobalOptimize cfg =
    let
        (ids, bbs_org) = unzip $ Map.toList $ getBasicBlocks cfg
        fs = splitWithFunction bbs_org
    in
        (++) (getHeader cfg) $ concat $ map globalOptimizeOnAFunction fs
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
                | a == "cltd" || a == "cltq" || a == "cqto" = if isLiv' "%rax" l then x : (elim tr lr) else elim tr lr
                | "set" `isPrefixOf` a = if isLiv b l then x : (elim tr lr) else elim tr lr
                | otherwise = x : (elim tr lr)
            elim _ _ = []

        isLiv r liv  = let fixed_r = rmRegIndex r in
            r `elem` liv || fixed_r `elem` liv
        isLiv' r liv = r `elem` (map rmRegIndex liv)


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
