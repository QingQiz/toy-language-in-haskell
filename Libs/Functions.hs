module Functions where

untilNoChange :: Eq a => (a -> a) -> a -> a
untilNoChange f x = until_no_change' f (f x) x where
    until_no_change' f x x' = if x == x' then x else until_no_change' f (f x) x

conn_inst cmd l r = ["\t" ++ cmd ++ "\t" ++ l ++ ", " ++ r]

isRegGroup c = let c' = dropWhile (`elem` "+-*/") c in
    cnt c' "()" == 2 && cnt c' "+-*/" == 0
    where
        cnt c l = foldl (\z x -> if x `elem` l then z + 1 else z) 0 c

