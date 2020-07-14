module RegisterAlloca where

import Livness
import Register
import Functions

import Data.Char
import Data.List
import Data.List.Split
import Data.List.Utils
import qualified Data.Map as Map


buildEnv ids tacs entries = unpack $ zipWith3 fix ids liv tacs where
    id_tac = Map.fromList $ zip ids tacs
    id_liv_final = livnessAnalysis tacs ids entries

    liv = (\id -> let tac = getItem id id_tac
                      liv = collectLivness tac (getItem id id_liv_final)
                  in  fixRegIndexInLiv tac liv) `map` ids
    unpack x =
        let (ids, livs, tacs) = unzip3 x
            id_tac = Map.fromList $ zip ids tacs
            livs'  = concatMap tail livs
            -- livs'' = head $ map rmDupItem $ fixLocal $ concat livs
            -- a register who is read before write is a local variable
            static = map (\x -> removeWhere ("rbp" `isInfixOf`) $ if null x then [] else head x) livs

            -- the last write of a local variable in a basic block shoule be pre-allocated
            func (id, sta) =
                let ups  = map fst $ filter (\(id', entry) -> id `elem` entry) $ zip ids entries
                    tacs = map (`getItem` id_tac) ups
                    sta' = map rmRegIndex sta
                in  concat [findLastWrite a b | a <- sta', b <- tacs]
                where findLastWrite r tac =
                          let (a, b) = break (\(a, b) -> rmRegIndex a == r) $ reverse tac
                          in  [fst $ head b | not $ null b]
            static' = rmDupItem $ concat $ (++) static $ zipWith (curry func) ids static
        in  ((map rmDupItem $ fixLocal livs'), concat tacs, first livs, static')

    first (l:ls) = if null l then first ls else head l

    fix id l t = (id, map (map doFix) l, map (\(a, b) -> (doFix a, doFix b)) t)
        where
            f x = "rsp" `isInfixOf` x || "rbp" `isInfixOf` x || "rip" `isInfixOf` x || isConst x

            doFix r | not $ isReg r = r
                    | isRegGroup  r = let (h:vals) = getGroupVal r in
                                          h ++ "(" ++ intercalate "," (map doFix vals) ++ ")"
                    | isSimple r    = if f r
                                      then rmRegIndex r
                                      else rmRegIndex r ++ show id ++ "0" ++ getRegIndex r
                    | otherwise     = let (a, b, op) = getOperand r in doFix a ++ op ++ doFix b

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

            findLast  r liv = length liv - 1 - findFirst r (reverse liv)

            step x liv = let b = findFirst x liv
                             e = findLast  x liv
                         in  if b /= e
                             then let (l, m, r) = splitAt3 (b + 1) e liv in
                                      l ++ map (x:) m ++ r
                             else liv


buildAllocaInit tac params static =
    let func_call = findReg $ findIdx tac 0 where
            findIdx ((a, b):t) n
                | a == "call" = let x = read $ last $ splitOn "#" b
                                in  map (n-) [1..(x+1)] ++ findIdx t (n + 1)
                | otherwise   = findIdx t (n + 1)
            findIdx [] _   = []
            findReg (i:is) | i >= 0 = let (a, _) = tac !! i in a : findReg is
                           | otherwise = findReg is
            findReg []     = []
        ret_val   = findReg $ findIdx tac 0 where
            findIdx ((a, b):t) n
                | "j" `isPrefixOf` a && "END" `isSuffixOf` b = (:) (n - 1) $ findIdx t (n + 1)
                | a == "leave"                               = (:) (n - 1) $ findIdx t (n + 1)
                | a == "ret"                                 = (:) (n - 1) $ findIdx t (n + 1)
                | otherwise                                  = findIdx t (n + 1)
            findIdx [] _ = []
            findReg (i:is) | i >= 0 = let (a, _) = tac !! i in
                if "rax" `isInfixOf` a then a : findReg is else findReg is
            findReg (i:is) | otherwise = findReg is
            findReg [] = []
        div       = findReg tac where
            findReg ((a, b):t)
                | '/' `elem` b = a : findReg t
                | otherwise = findReg t
            findReg [] = []
        g_vars    = filter ("rip" `isInfixOf`)
                    $ concatMap (\(a, b) -> getRegs a ++ getRegs b) tac
        fc_ret    = filter ("`fc" `isInfixOf`)
                    $ concatMap (\(a, b) -> getRegs a ++ getRegs b) tac
        final     = params ++ func_call ++ ret_val ++ g_vars ++ fc_ret ++ static ++ div
    in  Map.fromList $ zip final $ map fixRegIndex final
    where fixRegIndex r = let r' = rmRegIndex r
                          in  if isDigit $ last $ init r' then init r' else r'


registerRealloca tacs ids entries = func tac 0 alloc_init [] [] []
    where
        (liv, tac, params, static) = buildEnv ids tacs entries

        alloc_init = buildAllocaInit tac params static

        func (t:tac) li alloc tac' spin spout =
            let (t', alloc', spin', spout') = allocaAStep t li alloc
            in  func tac (li + 1) alloc' ((reverse t') ++ tac') (spin' : spin) (spout' : spout)
        func _ _ _ tac spin spout = (reverse tac, reverse spin, reverse spout)

        allocaAStep t@(tl, tr) li alloc =
            let regl = getRegs tl
                regr = getRegs tr
            in  allocaAStep' t (regl ++ regr) li alloc

        regFree l alloc =
            let valid     = filter (\(a, b) -> a `elem` l) $ Map.toList alloc
                reg_using = map snd valid
                reg_all   = map (!!0) registers
            in  filter (`notElem` reg_using) reg_all

        allocaAStep' t rs li alloc =
            let
                (alloc', spout) = foldr allocOne (alloc, []) rs
                (t', alloc'', spin, spout') = replaceRegWihtAlloca t alloc'
            in
                (t', alloc'', spin, spout' ++ spout)
            where
                l = liv !! li
                allocOne r (alloc, spout) =
                    let (alloc', spout') = tryAlloc r alloc
                    in  (alloc', mergeInto spout' spout)

                -- tryAlloc :: register -> alloc -> (alloc, register to spill out)
                tryAlloc r alloc  = case Map.lookup r alloc of
                    Nothing
                        | isRegGroup r && not ("rbp" `isInfixOf` r) ->
                              let (_:vs) = getGroupVal r
                                  -- FIXME spill out was droped
                              in  (foldr (\x z -> fst $ tryAlloc x z) alloc vs, Nothing)
                        | otherwise -> doAlloca r (reverse $ regFree l alloc) alloc
                    Just  x -> (alloc, Nothing)
                    where
                        doAlloca r regs alloc
                            | null regs =
                                  let reg_in_use = regh ++ regt
                                      (alloc', spout) = spillOut reg_in_use alloc
                                      (alloc'', _) = tryAlloc r alloc'
                                  in  (alloc'', Just spout)
                            | otherwise = deft
                            where regh  = getRegs (fst t)
                                  regt  = getRegs (snd t)
                                  deft  = (Map.insert r (head regs) alloc, Nothing)

                -- return (alloc', spill_out)
                spillOut regs alloc =
                    let
                        (a, b) = head $ dropWhile (\(a, b) -> a `elem` regs) $ Map.toList alloc
                        alloc' = case Map.lookup "spill" alloc of
                                     Nothing -> Map.insert "spill" "0" $ Map.insert a "spill@0" alloc
                                     Just  x -> let l  = splitOn "," x
                                                    l' = map read l
                                                    i  = show $ head $ dropWhile (`elem` l') [0..]
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
                                              l'  = liv !! (li - 1)
                                              free  = reverse $ regFree l' alloc
                                              (so, sa) = head same_name -- (orign, allocaed)
                                          in  ([(head free, sa), t'], Map.insert so (head free) alloc, Nothing, Nothing)
                                    where same_name = filter (\(bef, aft) -> bef /= reg && aft == x && bef `elem` l) $ Map.toList alloc
