module Register where

import Data.List
import Data.Map as Map hiding (map)

registers :: [[String]]
registers = [
    ["%rax", "%eax" , "%ax"  , "%al"  ],
    ["%rbx", "%ebx" , "%bx"  , "%bl"  ],
    ["%rdi", "%edi" , "%di"  , "%dil" ],
    ["%rsi", "%esi" , "%si"  , "%sil" ],
    ["%rdx", "%edx" , "%dx"  , "%dl"  ],
    ["%rcx", "%ecx" , "%cx"  , "%cl"  ],
    [ "%r8", "%r8d" , "%r8w" , "%r8b" ],
    [ "%r9", "%r9d" , "%r9w" , "%r9b" ],
    ["%r10", "%r10d", "%r10w", "%r10b"],
    ["%r11", "%r11d", "%r11w", "%r11b"],
    ["%r12", "%r12d", "%r12w", "%r12b"],
    ["%r13", "%r13d", "%r13w", "%r13b"],
    ["%r14", "%r14d", "%r14w", "%r14b"],
    ["%r15", "%r15d", "%r15w", "%r15b"]]

clr_reg r = "\tmovq\t$0, " ++ r

get_reg_index x = (!!) registers $ fromMaybe $ elemIndex True $ map (x `elem`) registers
    where fromMaybe (Just a) = a
          fromMaybe Nothing = error $ show x

get_low_reg x = last $ get_reg_index x
get_high_reg x = if last x == ')' || '`' `elem` x then x else head $ get_reg_index x

get_free_reg xs = registers !! ((\(Just x)->x) $ elemIndex True
    $ map (all ((==False) . (`isInfixOf` concat xs))) registers) !! 0

get_reg_offset :: String -> Int
get_reg_offset = read . takeWhile (/='(')

--                variable register datasize
type RegTable = Map String (String, Int)


empty_rgt :: Map String (String, Int)
empty_rgt = fromList [(".LC", ("", 0))]

get_label rgt = case Map.lookup ".label" rgt of
    Just (n, i) -> ".L_" ++ n ++ "_" ++ show i
    _ -> ".LL"

get_end_label rgt = case Map.lookup ".label" rgt of
    Just (n, i) -> ".L_" ++ n ++ "_END"
    _ -> ".LL"

update_label rgt = case Map.lookup ".label" rgt of
    Just (n, i) -> Map.insert ".label" (n, i + 1) rgt
    _ -> rgt
