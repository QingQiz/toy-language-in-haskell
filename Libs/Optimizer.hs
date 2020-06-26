module Optimizer where

import TAC
import CFG
import Livness
import Functions
import PeepHole

import Data.Char
import Data.List
import Data.List.Utils
import qualified Data.Map as Map


optimize code = untilF cond func code
    where cond now bef = length now > length bef || now == bef
          func         = ((\x -> untilNoChange instLower x) . swapInst . doGlobalOptimize . buildCFG)

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
        fromTAC optimized_tac ids entries
    where
        optimizeOnce tacs ids entries =
            let copy_prog  = map (\x -> untilNoChange (constFolding . copyPropagation) x) tacs
                dead_code1 = globalDeadCodeElim copy_prog ids entries
                expr_elim  = map (\x -> untilNoChange (constFolding . commonSubexprElim) x) dead_code1
                dead_code2 = globalDeadCodeElim expr_elim ids entries
                gcopy_prop = globalConstCopyPropagation dead_code2 ids entries
            in  gcopy_prop


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
        snd $ unzip $ Map.toList $ foldr forABlock (Map.fromList $ zip ids tacs) ids
    where
        ud = "ud" -- undefiend
        nc = "nc" -- not const
        id_entry = zip ids entries

        findConst tac = Map.toList $ Map.fromList -- find the last assign
            $ map (\(a, b) -> (rmRegIndex a, b)) $ sort
            -- find all (rbp, const)
            $ filter (isConst . snd) tac

        forABlock id id_tac =
            let consts = findConst (getItem id id_tac)
            in  foldr forAConst id_tac consts

        -- forAConst :: (lst, rst) -> id_tac -> id_tac'
        forAConst (a, b) id_tac =
            let stat_init = Map.fromList
                    $ map (\x@(id, _) -> (,) id (ud, buildStat ud x))
                    $ Map.toList id_tac
                stat = untilNoChange updateStat stat_init
            in  doReplace (a, b) stat id_tac
            where
                doReplace (a, b) id_stat id_tac = Map.fromList $ map doReplace' (Map.toList id_tac) where
                    doReplace' (id, tac)
                        | bef == ud || bef == nc = (id, tac)
                        | otherwise = (id, func a bef tac)
                        where (bef, aft) = getItem id id_stat
                              func a b inp@((l, r):tac)
                                  | rmRegIndex a == rmRegIndex l = inp
                                  | rmRegIndex a == rmRegIndex r = (l, b) : func a b tac
                                  | otherwise = (l, r) : func a b tac
                              func _ _ _ = []

                -- update stat by tac of current basic block
                buildStat init (id, tac)
                    | otherwise =
                          let l     = filter (\(ca, cb) -> rmRegIndex ca == rmRegIndex a) tac
                              b'    = if null l then ud else snd $ last l
                          in  meet init b'

                -- update stat by stat of other basic block
                updateStat id_stat = Map.fromList $ map (\id -> (,) id $ updateStat' id) ids where
                    updateStat' id =
                        let fs = map fst $ filter (\(i, e) -> id `elem` e) id_entry
                            st = map (\id -> snd $ getItem id id_stat) fs
                            (bef, aft) = getItem id id_stat
                            bef' = foldr (\x z -> meet' x z) (if null st then ud else head st) st
                            aft' = buildStat bef' (id, getItem id id_tac)
                        in  (bef', aft')
                        where meet' a b = if a == ud || b == ud then ud else meet a b

        meet a b | a == nc || b == nc = nc
                 | a == ud = b
                 | b == ud = a
                 | a /= b  = nc
                 | a == b  = a


copyPropagation tac = untilNoChange (\x -> cP x [] Map.empty) tac where
    cP (c:cs) res eq = cP cs ((doReplace c eq) : res) (update c eq)
    cP [] res _ = reverse res

    update c@(a, b) eq
        | b == "" || "%rbp" `isPrefixOf` a || "%rax`fc" `isPrefixOf` b = eq
        | a == "call" = Map.fromList
                        $ filter (\t -> notInTup "rax" t && notInTup "rip" t)
                        $ Map.toList eq
        | "set" `isPrefixOf` a =
              Map.fromList $ filter (\t -> notInTup b t) $ Map.toList eq
        | '%' `notElem` a = eq
        | isNotArith b = let res = Map.insert a b eq
                         in  if "rip" `isInfixOf` b && head b /= '*'
                             then Map.insert ("*(" ++ a ++ ")0") ('*':b) res
                             else res
        | otherwise = eq
        where notInTup x (a, b) = (not . isInfixOf x) a && (not . isInfixOf x) b

    doReplace x@(a, b) eq
        | null b = x
        | "set" `isPrefixOf` a = x
        | otherwise = (a, doCopy b eq)


commonSubexprElim tac = untilNoChange (\x -> cE x [] Map.empty) tac where
    cE (c:cs) res eq = cE cs ((doReplace c eq) : res) (update c eq)
    cE [] res _ = reverse res

    update (a, b) eq
        | b == "" = eq
        | a == "call" = Map.fromList
                        $ filter (\t -> notInTup "rax" t && notInTup "rip" t)
                        $ Map.toList eq
        | "set" `isPrefixOf` a =
              Map.fromList $ filter (\t -> notInTup b t) $ Map.toList eq
        | '%' `notElem` b  || isLetter (head a) = eq
        | isReg a && isReg b = let res = Map.insert b a eq
                               in  if "rip" `isInfixOf` b && head b /= '*'
                                   then Map.insert ('*':b) ("*(" ++ a ++ ")0") res
                                   else res
        | otherwise = eq
        where notInTup x (a, b) = (not . isInfixOf x) a && (not . isInfixOf x) b

    doReplace c@(a, b) eq
        | b == "" = c
        | "set" `isPrefixOf` a = c
        | otherwise = case Map.lookup b eq of
              Nothing | isRegGroup b && head b == '*' -> case Map.lookup (tail b) eq of
                            Nothing -> c
                            Just  x -> (a, "*" ++ x)
                      | otherwise -> c
              Just  x -> (a, x)


doCopy c eq = doCopy' c
    where
        copyIntoG (c:cs) z = case copy c eq of
            Nothing -> copyIntoG cs (c:z)
            Just  x -> if isRegGroup x then copyIntoG cs (c:z) else copyIntoG cs (x:z)
        copyIntoG [] z = "(" ++ intercalate "," (reverse z) ++ ")"

        doCopy' x = case getOperand x of
            (a, "", _) -> case copy a eq of
                Nothing ->
                    if isRegGroup a
                    then let (h:val) = getGroupVal a in h ++ copyIntoG val [] ++ (tail $ snd $ break (==')') a)
                    else a
                Just  x -> x
            (a, b, op) -> doCopy' a ++ op ++ doCopy' b

        copy a eq | a == "" || "%rsp" `isPrefixOf` a = Nothing
                  | otherwise = case Map.lookup a eq of
                        Nothing -> if head a == '*'
                            then case Map.lookup (tail a) eq of
                                     Nothing -> Nothing
                                     Just  x -> let x' = mayCopy x (tail a) in Just $ '*':x'
                            else Nothing
                        Just  x -> Just $ mayCopy x a
        mayCopy x y = if isSimple x
                      then x
                      else if isNotArith c
                           then x
                           else y


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
