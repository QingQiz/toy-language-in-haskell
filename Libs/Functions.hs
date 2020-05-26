module Functions where

import qualified Data.Map as Map


untilNoChange :: Eq a => (a -> a) -> a -> a
untilNoChange f x = until_no_change' f (f x) x where
    until_no_change' f x x' = if x == x' then x else until_no_change' f (f x) x

conn_inst cmd l r = ["\t" ++ cmd ++ "\t" ++ l ++ ", " ++ r]

isRegGroup c = let c' = dropWhile (`elem` "+-*/") c in
    cntElem "()" c' == 2 && cntElem "+-*/" c' == 0


cnt c = foldl (\z x -> if x == c then z + 1 else z) 0
cntElem c = foldl (\z x -> if x `elem` c then z + 1 else z) 0

inMap a m = case Map.lookup a m of
    Just _ -> True
    _ -> False

elemWhere f l = case snd $ break f l of
    [] -> error $ "\nelem can not found" ++ " in " ++ show l
    x  -> head x

