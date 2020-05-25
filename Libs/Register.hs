module Register where

import Data.List
import Data.Map as Map hiding (map)

registers :: [[String]]
registers = [
    ["%rax", "%eax",  "%ax",  "%al"],
    ["%rdi", "%edi",  "%di", "%dil"],
    ["%rsi", "%esi",  "%si", "%sil"],
    ["%rdx", "%edx",  "%dx",  "%dl"],
    ["%rcx", "%ecx",  "%cx",  "%cl"],
    [ "%r8", "%r8d", "%r8w", "%r8b"],
    [ "%r9", "%r9d", "%r9w", "%r9b"]]

clr_reg r = "\tmovl\t$0, " ++ r

get_reg_index x = (!!) registers $ fromMaybe $ elemIndex True $ map (x `elem`) registers
    where fromMaybe (Just a) = a
          fromMaybe Nothing = error $ show x

get_low_reg x = last $ get_reg_index x
get_high_reg x = if last x == ')' then x else head $ get_reg_index x

get_free_reg xs = registers !! ((\(Just x)->x) $ elemIndex True
    $ map (all (==False) . map (`isInfixOf` (concat xs))) registers) !! 1

get_reg_offset :: String -> Int
get_reg_offset = read . fst . break (=='(')

--                variable register datasize
type RegTable = Map String (String, Int)


empty_rgt :: Map String (String, Int)
empty_rgt = fromList []

get_label rgt = case Map.lookup ".label" rgt of
    Just (n, i) -> ".L_" ++ n ++ "_" ++ show i
    _ -> ".LL"

get_end_label rgt = case Map.lookup ".label" rgt of
    Just (n, i) -> ".L_" ++ n ++ "_END"
    _ -> ".LL"

update_label rgt = case Map.lookup ".label" rgt of
    Just (n, i) -> Map.insert ".label" (n, i + 1) rgt
    _ -> rgt

