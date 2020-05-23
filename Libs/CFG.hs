module CFG where

import Data.List
import qualified Data.Map as Map


type IdBasicBlock = Map.Map Int BasicBlock

type AsmHeader  = [String]
type AsmCode    = [String]

data CFG = CFG AsmHeader IdBasicBlock
    deriving (Show, Eq)

data BasicBlock = BasicBlock AsmCode [Int] | BadBlock
    deriving (Show, Eq)


buildCFG :: AsmCode -> CFG
buildCFG code =
    let
        (h:xs) = splitWithFunction code
        fs = map splitWithJumpLabel xs
        -- convert code to BasicBlock
        bbs = concat $ map toBasicBlock $ makeId fs
    in
        CFG h (Map.fromList bbs)
    where
        -- assign id to each code block
        makeId l = zipWith bind len l where
            bind len l = zip [len..] l

            len = let l' = prefixSum $ map length l in
                map (\x -> x - head l') l'

            prefixSum l = reverse $ prefixSum' l 0 [] where
                prefixSum' (x:xs) z l = prefixSum' xs (z+x) $ (z+x):l
                prefixSum' [] _ l = l

        -- convert code blocks to BasicBlocks
        toBasicBlock bs =
            let
                jmp = reverse $ findJmp bs []
                block_to = (zipWith findLabelId (init bs) jmp) ++ [[fst $ head bs]]
                bbs = bind block_to bs
                fixed_bbs = init bbs ++ (\(BasicBlock x t) -> [BasicBlock x []]) (last bbs)
            in
                zip (fst $ unzip bs) fixed_bbs
            where
                -- find jump target (label name)
                findJmp (b:bs) z = findJmp bs $ (snd $ break (=='.') (last (snd b))) : z
                findJmp _ z = z

                -- find the block to jump (or fall through to next block)
                findLabelId b label =
                    if label == ""
                    then [fst b + 1]
                    else (fst $ head $ snd $ break (\inp -> head (snd inp) == label ++ ":") bs)
                         : (if "jmp" `isInfixOf` (last (snd b)) then [] else [fst b + 1])

                -- convert code block and its jump target to BasicBlock
                bind block_to bs = zipWith bind' block_to bs where
                    ids = concat block_to
                    bind' entry (id, code) = if id `notElem` ids then BadBlock else BasicBlock code entry

        splitWithFunction code = reverse $ map reverse $ foldl step [[]] code where
            step z@(x:xs) c = if head c `notElem` ".\t" then [c] : z else (c:x):xs

        splitWithJumpLabel code = foldr step [[]] code where
            step c z@(x:xs) = case c of
                '.':cs -> []:(c:x):xs
                '\t':'j':cs -> if null x then [c]:xs else [c]:z
                _ -> (c:x):xs


getBlockById id (CFG _ blks) = (\(Just x) -> x) $ Map.lookup id blks

