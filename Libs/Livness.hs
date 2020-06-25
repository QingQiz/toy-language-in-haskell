module Livness where

import Debug.Trace
import Register
import Functions
import Data.List
import Data.Char
import Data.List.Split
import qualified Data.Map as Map



show' ((a, b):ts) = "\n" ++ (if length a >= 8 then a ++ "" else a ++ "\t") ++ "\t" ++ (if null b then "" else "=\t" ++ b) ++ show' ts
show' [] = "\n"

livnessAnalysis tacs ids entries =
    let
        g_vars = filter (\x -> "rip" `isInfixOf` x)
            $ concat
            $ map (\(a, b) -> getRegs a ++ getRegs b)
            $ concat tacs
        id_liv = (\inp -> Map.insert (last ids) (getItem (last ids) inp ++ ["%rax", "%rsp", "%rbp"] ++ g_vars) inp)
            $ Map.fromList
            $ zipWith findGV ids
            $ replicate (length ids) []

        findGV id _ = (id, findGVar (getItem id id_tac)) where
            findGVar ((a, b):ts)
                | isRegGroup a && not ("rbp" `isInfixOf` a) = a : findGVar ts
                | otherwise = findGVar ts
            findGVar _ = []
        res = untilNoChange (buildLiveness (head tacs) (head ids) (head entries) []) id_liv
    in
        trace ("\n---------------------\n" ++ show' (concat tacs) ++ "\n\n"++ show res ++ "\n---------------------\n") res

        -- res
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
        | a == "call"          = let n =  read $ last $ splitOn "#" b
                                 in  "%rax" : (map fixRegIndex $ take n $ tail $ tail $ map (!!0) registers)
                                     ++ specialRemove' "%rax`fc" init
        | a == "cltd"          = "%rax" : init
        | a == "cltq"          = "%rax" : init
        | a == "cqto"          = "%rax" : init
        | a == "test"          = b : init
        | otherwise            =
              let header = (if isRegGroup a then tail $ getGroupVal a else [])
                           ++ (if head a == '*' then tail $ getRegs $ tail a else [])
                  init'  = specialRemove a init
                  body   = case getOperand b of
                      (a, [], []) -> func a
                      (a, b , _ ) -> func a ++ func b
              in  header ++ body ++ init'
              where func a | "rbp" `isInfixOf` a = [a]
                           | isRegGroup a && head a == '*' = tail $ getRegs a
                           | otherwise = getRegs a

    specialRemove a l = let fixed_reg = rmRegIndex a in
        removeWhere (\x -> x == fixed_reg || x == a) l
    specialRemove' a l = removeWhere (\x -> rmRegIndex x == a) l

    fixRegIndex r = if isDigit $ last r then r ++ "x" else r


fixRegIndexInLiv tac liv =
    let m_init = Map.fromList $ zip regs [0,0..]
        regs   = (["%rsp", "%rbp"] ++ map (!!0) registers)
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
