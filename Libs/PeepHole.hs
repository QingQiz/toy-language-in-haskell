module PeepHole where

import Register
import Functions

import Data.List
import Data.List.Split
import Data.List.Utils


finalDash code =
    let
        heap_saver = map (\x -> "\t.comm\t." ++ tail x ++ ",8") (map (!!0) registers)
        func_print = ["\t.globl\tprint"]
            ++ conn_lab    "print"
            ++ conn_inst_s "pushq" "%rbp"
            ++ conn_inst   "movq" "%rsp" "%rbp"
            ++ conn_inst   "movq" "%rdi" ".rdi(%rip)"
            ++ conn_inst   "andq" "$-16" "%rsp"
            ++ conn_inst_s "call" "printf@PLT"
            ++ conn_inst   "movq" ".rdi(%rip)" "%rdi"
            ++ conn_cmd    "leave"
            ++ conn_cmd    "ret"
        func_read  = ["\t.globl\tread"]
            ++ conn_lab    "read"
            ++ conn_inst_s "pushq" "%rbp"
            ++ conn_inst   "movq" "%rsp" "%rbp"
            ++ conn_inst   "movq" "%rdi" ".rdi(%rip)"
            ++ conn_inst   "subq" "$8" "%rsp"
            ++ conn_inst   "andq" "$-16" "%rsp"
            ++ conn_inst   "leaq" "(%rsp)" "%rsi"
            ++ conn_inst_s "call" "scanf@PLT"
            ++ conn_inst   "movq" "(%rsp)" "%rax"
            ++ conn_inst   "movq" ".rdi(%rip)" "%rdi"
            ++ conn_cmd    "leave"
            ++ conn_cmd    "ret"
    in  heap_saver ++ func_read ++ func_print ++ (saveRegs $ fixCode code)
    where
        fixCode (c:cs)
            | isCommd "set" c' && "%r" `isPrefixOf` tgt =
                  conn_inst_s cmd tgt_low ++ conn_inst "movzbq" tgt_low tgt ++ fixCode cs
            | otherwise = c' : fixCode cs
            where c' = removeSig c
                  cmd = getCommd c'
                  tgt = getCommdTarget c'
                  tgt_low = get_low_reg tgt
        fixCode [] = []

        removeSig c = replace "`fc" "" $ head $ splitOn "#" c

        collectRegsW code = collect code []
            where
                collect (c:cs) res
                    | ',' `notElem` c   = collect cs res
                    | isRegGroup target = collect cs res
                    | otherwise         = collect cs (target : res)
                    where target = last $ splitOn ", " c
                collect _ res = rmDupItem
                    $ filter (\x -> x /= "%rbp" && x /= "%rsp" && x /= "%rax" && "%r" `isPrefixOf` x) res

        saveToStack regs = (map (\x -> "\tpushq\t" ++ x) regs, map (\x -> "\tpopq\t" ++ x) $ reverse regs)
        saveToHeap  regs = ((map (\x -> "\tmovq\t" ++ x ++ ", " ++ toHeap x) regs)
                           ,(map (\x -> "\tmovq\t" ++ toHeap x ++ ", " ++ x) regs))
            where toHeap r = "." ++ tail r ++ "(%rip)"

        saveRegForAFunc code@(c:cs) =
            let regs   = collectRegsW code
                tStack = saveToStack regs
                tHeap  = saveToHeap  regs
            in  (:) c $ case cs of
                (a@"\tpushq\t%rbp":b@"\tmovq\t%rsp, %rbp":c':cs')
                    | "subq" `isInfixOf` c' && "%rsp" `isInfixOf` c' ->
                          (a:b:c':[]) ++ fst tStack ++ cs'_body      ++ snd tStack ++ cs'_next
                    | otherwise ->
                          (a:b:[])    ++ fst tStack ++ (c':cs'_body) ++ snd tStack ++ cs'_next
                    where  (cs'_body, cs'_next) = break (=="\tleave") cs'
                cs' -> let (cs'_body, cs'_next) = break (\x -> x == "\tleave" || x == "\tret") cs'
                       in  fst tHeap ++ cs'_body ++ snd tHeap ++ cs'_next
        saveRegs code =
            let (h:fs) = splitWithFunction code
            in  foldr (++) [] (h : map saveRegForAFunc fs)


reduceStackChange code
    | null ref = reduceAllStack code
    | null neg_ref = reduceSubRspAndLeave code
    | otherwise = code
    where ref     = filter (\x ->  "(%rbp" `isInfixOf` x
                               || ("push"  `isInfixOf` x && not ("\t%rbp" `isInfixOf` x))
                               ||  "call" `isInfixOf` x) code
          neg_ref = filter (\x -> "\t-"    `isInfixOf` x || " -"   `isInfixOf` x) ref

          reduceAllStack = reduce1 (\x -> "rsp" `isInfixOf` x || "rbp" `isInfixOf` x || "leave" `isInfixOf` x)
          reduceSubRspAndLeave = reduce1 (\x -> ("sub" `isInfixOf` x && "rsp" `isInfixOf` x))


reduceUselessJmp = reduce2 f where
    f c cn = head cn == '.' && "\tj" `isPrefixOf` c && init cn `isSuffixOf` c


reduceUselessCmp = reduce2 f where
    f c cn = ("\tcmp" `isPrefixOf` c || "\ttest" `isPrefixOf` c)
      && not ("\tj"  `isPrefixOf` cn || "\tset" `isPrefixOf` cn)


reduce2 _ [] = []
reduce2 f code = reduce code (tail code) where
    reduce (c:cs) (cn:cns) | f c cn = reduce cs cns
                           | otherwise = c : reduce cs cns
    reduce c [] = c

reduce1 f (c:code) | f c = reduce1 f code
                   | otherwise = c : reduce1 f code
reduce1 _ _ = []
