module Register where

import Data.Map as Map

registers :: [[String]]
registers = [
    ["%rax", "%eax",  "%ax",  "%al"],
    ["%rdi", "%edi",  "%di", "%dil"],
    ["%rsi", "%esi",  "%si", "%sil"],
    ["%rdx", "%edx",  "%dx",  "%dl"],
    ["%rcx", "%ecx",  "%cx",  "%cl"],
    [ "%r8", "%r8d", "%r8w", "%r8b"],
    [ "%r9", "%r9d", "%r9w", "%r9b"]]


--                variable register datasize
type RegTable = Map String (String, Int)


empty_rgt :: Map String (String, Int)
empty_rgt = fromList []

get_reg_offset :: String -> Int
get_reg_offset = read . fst . break (=='(')

get_label rgt = case Map.lookup ".label" rgt of
    Just (n, i) -> ".L_" ++ n ++ "_" ++ show i
    _ -> ".LL"

update_label rgt = case Map.lookup ".label" rgt of
    Just (n, i) -> Map.insert ".label" (n, i + 1) rgt
    _ -> rgt

