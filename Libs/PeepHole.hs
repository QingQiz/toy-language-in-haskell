module PeepHole where

import Register
import Functions

import Data.List
import Data.List.Split
import Data.List.Utils


finalDash code =
    let
        reg_sav = saveToHeap $ take 7 $ drop 1 $ map (!!0) registers
        heap_saver = map (\x -> "\t.comm\t." ++ tail x ++ ",8") (map (!!0) registers)
        func_print = ["\t.globl\tprint"]
            ++ conn_lab    "print"
            ++ conn_inst_s "pushq" "%rbp"
            ++ conn_inst   "movq" "%rsp" "%rbp"
            ++ fst reg_sav
            ++ conn_inst   "andq" "$-16" "%rsp"
            ++ conn_inst_s "call" "printf@PLT"
            ++ snd reg_sav
            ++ conn_cmd    "leave"
            ++ conn_cmd    "ret"
        func_read  = ["\t.globl\tread"]
            ++ conn_lab    "read"
            ++ conn_inst_s "pushq" "%rbp"
            ++ conn_inst   "movq" "%rsp" "%rbp"
            ++ fst reg_sav
            ++ conn_inst   "subq" "$8" "%rsp"
            ++ conn_inst   "andq" "$-16" "%rsp"
            ++ conn_inst   "leaq" "(%rsp)" "%rsi"
            ++ conn_inst_s "call" "scanf@PLT"
            ++ conn_inst   "movq" "(%rsp)" "%rax"
            ++ snd reg_sav
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


swapInst (c1:c2:c3:cs)
    | head c1 /= '\t' = c1 : swapInst (c2:c3:cs)
    | head c2 /= '\t' = c1 : c2 : swapInst (c3:cs)
    | head c3 /= '\t' = c1 : c2 : c3 : swapInst cs
    | b1 == "" = c1 : swapInst (c2:c3:cs)
    | b2 == "" = c1 : c2 : swapInst (c3:cs)
    | b3 == "" = c1 : c2 : c3 : swapInst cs
    | b1 == b3 && b2 /= b3
      && (not . isInfixOf b1) a2
      && (not . isInfixOf b2) a1
      && (not . isInfixOf b1) b2
      && (not . isInfixOf b2) b1 = (c2 : c1 : c3 : swapInst cs)
    | otherwise = c1 : swapInst (c2:c3:cs)
    where
        (cmd1, a1, b1) = getOperandFromCode c1
        (cmd2, a2, b2) = getOperandFromCode c2
        (cmd3, a3, b3) = getOperandFromCode c3
swapInst x = x


instLower (c1:c2:cs) = let merged = mergeCalc c1 c2
                       in  if null merged
                           then c1 : instLower (c2 : cs)
                           else merged ++ instLower cs
instLower x = x

mergeCalc c1 c2
    | head c1 /= '\t' = []
    -- mov  $2, %r
    -- imul %s, %r  -> lea (,%s,2), %r
    | b1 == b2 && cmd1 == "movq" && isConst a1 && cmd2 == "imulq" && isSimpleReg a2 && validConst a1 =
          conn_inst "leaq" (lea "" "" a2 $ tail a1) b1
    -- mov  $s, %r
    -- imul $2, %r  -> lea (,%s,2), %r
    | b1 == b2 && cmd1 == "movq" && isSimpleReg a1 && cmd2 == "imulq" && isConst a2 && validConst a2 =
          conn_inst "leaq" (lea "" "" a1 $ tail a2) b1
    -- mov  $2, %r
    -- add  %s, %r  -> lea 2(%s), %r
    | b1 == b2 && cmd1 == "movq" && isConst a1 && cmd2 == "addq" && isSimpleReg a2 =
          conn_inst "leaq" (tail a1 ++ "(" ++ a2 ++ ")") b1
    -- mov  %s, %r
    -- add  $2, %r  -> lea 2(%s), %r
    | b1 == b2 && cmd1 == "movq" && isSimpleReg a1 && cmd2 == "addq" && isConst a2 =
          conn_inst "leaq" (tail a2 ++ "(" ++ a1 ++ ")") b1
    -- mov  %s, %r
    -- sub  $2, %r  -> lea -2(%s), %r
    | b1 == b2 && cmd1 == "movq" && isSimpleReg a1 && cmd2 == "subq" && isConst a2 =
          conn_inst "leaq" (negConst a2 ++ "(" ++ a1 ++ ")") b1
    -- mov  %s, %r
    -- add  %t, %r  -> lea (%t,%s), %r
    | b1 == b2 && cmd1 == "movq" && isSimpleReg a1 && cmd2 == "addq" && isSimpleReg a2 =
          conn_inst "leaq" ("(" ++ a1 ++ "," ++ a2 ++ ")") b1
    -- lea (_), %r
    -- add $2, %r          -> lea 2(_), %r
    | b1 == b2 && cmd1 == "leaq" && "rip" `isInfixOf` a1 = []
    | b1 == b2 && cmd1 == "leaq" && lack1 a1 && cmd2 == "addq" && isConst a2 =
          conn_inst "leaq" (merge1 (tail a2) a1) b1
    -- lea (_), %r
    -- sub $2, %r          -> lea 2(_), %r
    | b1 == b2 && cmd1 == "leaq" && lack1 a1 && cmd2 == "subq" && isConst a2 =
          conn_inst "leaq" (merge1 (negConst a2) a1) b1
    -- lea x(%s), %r
    -- add %t, %r          -> lea x(%s,%t), %r
    | b1 == b2 && cmd1 == "leaq" && lack3 a1 && cmd2 == "addq" && isSimpleReg a2 =
          conn_inst "leaq" (merge3 a2 a1) b1
    -- lea _(,%s,x), %r
    -- add %t, %r          -> lea _(%t,%s,x), %r
    | b1 == b2 && cmd1 == "leaq" && lack2 a1 && cmd2 == "addq" && isSimpleReg a2 =
          conn_inst "leaq" (merge2 a2 a1) b1
    -- lea (,%r1,x), %r
    -- lea _(_,%r), %r   -> lea _(_,%r1,x), %r
    | b1 == b2 && cmd1 == "leaq" && cmd2 == "leaq" && lack1 a1 && lack2 a1 && lack4 a2 && pos2 a2 == b1 =
          conn_inst "leaq" (mergeL a2 a1) b1
    | otherwise = []
    where
        (cmd1, a1, b1) = getOperandFromCode c1
        (cmd2, a2, b2) = getOperandFromCode c2

        negConst c = show $ negate $ read $ tail c
        validConst a = a `elem` ["$1", "$2", "$4", "$8"]
        lea a b c d = a ++ "(" ++ b ++ "," ++ c ++ "," ++ d ++ ")"

        isSimpleReg x = isReg x && isSimple x

        lack1 a = head a == '('
        lack2 a = getGroupVal a !! 1 == ""
        lack3 a = length (getGroupVal a) == 2
        lack4 a = length (getGroupVal a) == 3

        pos2  a = getGroupVal a !! 2

        merge1 a b = a ++ b
        merge2 a b = let (x1:x2:x3:x4:[]) = getGroupVal b
                     in  lea x1 a x3 x4
        merge3 a b = init b ++ "," ++ a ++ ")"
        mergeL a b = (fst $ break (==',') a) ++ (snd $ break (==',') b)


getOperandFromCode c = (getCommd c, a, b) where
    (a, b) = case splitOn ", " $ last $ splitOn "\t" c of
        (x:y:_) -> (x, y)
        (x:[]) -> (x, "")
        _ -> ("", "")
