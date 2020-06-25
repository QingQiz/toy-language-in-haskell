module CodeGen where

import Ast
import Register
import Functions

import Data.Char
import Data.List
import Data.List.Utils
import Data.List.Split
import qualified Data.Map as Map

runCodeGen = cProgram

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
            | isCommd "set" c' = let cmd = getCommd c'
                                     tgt = getCommdTarget c'
                                     tgt_low = get_low_reg tgt
                                 in  conn_inst_s cmd tgt_low ++ conn_inst "movzbq" tgt_low tgt ++ fixCode cs
            | otherwise = c' : fixCode cs
            where c' = removeSig c
        fixCode [] = []

        removeSig c = replace "`fc" "" $ head $ splitOn "#" c

        collectRegsW code = collect code []
            where
                collect (c:cs) res
                    | isCommd "ret" c   = res
                    | ',' `notElem` c   = collect cs res
                    | isRegGroup target = collect cs res
                    | otherwise         = collect cs (target : res)
                    where target = last $ splitOn ", " c
                collect _ res = res

        saveToStack regs = let regs' = filter (/="%rax") $ rmDupItem regs
                           in  (map (\x -> "\tpushq\t" ++ x) regs', map (\x -> "\tpopq\t" ++ x) $ reverse regs')
        saveToHeap  regs =
            let regs' = filter (/="%rax") $ rmDupItem regs
            in  ((map (\x -> "\tmovq\t" ++ x ++ ", " ++ toHeap x) regs'),
                 (map (\x -> "\tmovq\t" ++ toHeap x ++ ", " ++ x) regs'))
            where toHeap r = "." ++ tail r ++ "(%rip)"

        saveRegForAFunc code@(c:cs) =
            let regs   = filter (\x -> x /= "%rbp" && x /= "%rsp") $ collectRegsW code
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


cProgram :: Ast -> [String]
cProgram (Program _ _ vds fds) = concat $ bind (cGlobalVarDef vds) (cFuncDefs fds)
    where
        bind (a, b) c = bind' a (c b)
        bind' a (b, c, d) = [a ++ c ++ d, b]


cGlobalVarDef :: [Ast] -> ([String], RegTable)
cGlobalVarDef defs = foldr step ([], empty_rgt) defs where
    step (VarDef t _ xs) (l, rgt) =
        let
            rst = case t of
                TInt  -> foldr (step' 8) ([], empty_rgt) xs
                TChar -> foldr (step' 1) ([], empty_rgt) xs
        in
            (fst rst ++ l, Map.union (snd rst) rgt)

    step' s (Identifier _ i) (l, rgt) =
        (("\t.comm\t" ++ i ++ "," ++ show 8) : l, Map.insert i (i ++ "(%rip)" , s) rgt)
    step' s (Array _ (Identifier _ i) (Number _ n)) (l, rgt) =
        (("\t.comm\t" ++ i ++ "," ++ show (n * 8)) : l, Map.insert i (i ++ "(%rip)", s) rgt)

--                                 code   all_func_name   static strings
cFuncDefs :: [Ast] -> RegTable -> ([String], [String], [String])
cFuncDefs fds rgt =
    let (funcs, rgt') = foldr step ([], rgt) fds
        cfds          = map collect_fn fds
    in  (concat funcs, cfds, concat $ collect_st rgt')
    where
        step fd (res, rgt) = let (c, r) = cAFuncDef fd rgt
                             in  (c:res, r)
        collect_fn (FuncDef _ _ (Identifier _ fn) _ _) = ("\t.globl\t" ++ fn)
        collect_st r = let m = filter (\(a, b) -> head a == ':') $ Map.toList r
                       in  map (\(a, (b, _)) -> let b' = replace "(%rip)" "" b
                                                in  [b' ++ ":"] ++ ["\t.string\t\"" ++ tail a ++ "\""]) m


cAFuncDef :: Ast -> RegTable -> ([String], RegTable)
cAFuncDef (FuncDef ft _ (Identifier _ fn) pl fb) rgt =
    let res = bind (cParams pl) (cComdStmt fb)
    in  ([fn ++ ":", "\tpushq\t%rbp", "\tmovq\t%rsp, %rbp"]
             ++ fst res
             ++ ["\tret"], Map.union (snd res) rgt)
    where
        -- clear %eax if function does not have a return-stmt
        label_end = [clr_reg "%rax", ".L_" ++ fn ++ "_END:"]
        bind (a, b) c     = bind' a $ c $ Map.insert ".label" (fn, 1) $ Map.union b rgt
        bind' a (b, c, d) = if c /= 0
                            then (["\tsubq\t$" ++ show c ++ ", %rsp"] ++ a ++ b ++ label_end ++ ["\tleave"], getStrObj d)
                            else (a ++ b ++ label_end ++ ["\tleave"], getStrObj d)

        getStrObj rgt = Map.fromList $ filter (\(a, b) -> a == ".LC" || head a == ':') $ Map.toList rgt
        cParams pls = (for_pl pls, Map.fromList $ get_param_reg pls)

        get_param_reg pls =
            let l2 = unzip $ map (\(t, Identifier _ i) -> ((\t -> case t of TInt -> 8; TChar -> 1) t, i)) pls
                regs = map (\a -> show a ++ "(%rbp)") $ [-8,-16..(-48)] ++ [16,24..]
            in zip (snd l2) $ zip regs $ fst l2 -- (name, (reg, size))

        for_pl pls =
            let pl = get_param_reg pls
                si = unzip $ map (\(_,(_,c))->case c of 8->(0,"movq"); 1->(0,"movq")) pl
                reg = map (\inp->(registers!!inp)!!(fst si!!(inp-2))) [2..7]
            in map (\(a,b,c)->"\t" ++ a ++ "\t" ++ b ++ ", " ++ c) $ zip3 (snd si) reg $ map (\(_,(b,_))->b) pl


cComdStmt :: Ast -> RegTable -> ([String], Int, RegTable) -- Int: stack size needed
cComdStmt (ComdStmt _ [] vd sl) rgt =
    let (clr, var, siz) = cLocalVar vd rgt
        (sl', rgt')     = cStmtList sl var
    in  ((++) clr sl', siz, rgt')


cLocalVar :: [Ast] -> RegTable -> ([String], RegTable, Int) -- Int: stack size
cLocalVar vds rgt =
    let
        offset :: [Int]
        offset = map (get_reg_offset . head)
            $ filter (not . null)
            $ map (fst . span ("(%rbp)" `isSuffixOf`) . tails)
            $ map (\(_, (b,_))->b) $ Map.toList rgt

        int_var = map trans $ concat $ map take_name [x | x <- vds, is_int x]
        chr_var = map trans $ concat $ map take_name [x | x <- vds, not $ is_int x]

        int_rgt = for_siz 8 int_var (if null offset then 0 else minimum offset) []
        chr_rgt = for_siz 1 chr_var (fst int_rgt) []

        regs = fst . unzip . snd . unzip . Map.toList . snd
        clr_var = concat $ map (conn_inst "movq" "$0") $ (regs int_rgt) ++ (regs chr_rgt)
    in
        (clr_var, Map.union (Map.union (snd int_rgt) (snd chr_rgt)) rgt, (abs $ fst chr_rgt - 15) `div` 16 * 16)
    where
        is_int (VarDef TInt _ _)  = True
        is_int (VarDef TChar _ _) = False

        trans (Array _ (Identifier _ i) (Number _ n)) = (i, n)
        trans (Identifier _ i) = (i, 1)

        take_name = \(VarDef _ _ n) -> n

        for_siz siz' ((name, len):xs) oft res =
            for_siz siz xs (oft - siz - len * siz) $ (name, ((show $ oft - len * siz) ++ "(%rbp)", siz')) : res
            where siz = 8
        for_siz _ [] oft res = (oft, Map.fromList res)


cStmtList :: Ast -> RegTable -> ([String], RegTable)
cStmtList (StmtList _ sl) rgt = cStmts sl rgt where
    cStmts (sl:sls) rgt = bind (cAStmt sl rgt) (cStmts sls)
    cStmts [] rgt = ([], rgt)

    bind (a, b) c = bind' a (c b)
    bind' a (b, c) = (a ++ b, c)


cAStmt :: Ast -> RegTable -> ([String], RegTable)
cAStmt sl@(StmtList _ _) rgt = cStmtList sl rgt
cAStmt Empty rgt = ([], rgt) -- or return nop

cAStmt (IfStmt _ c s Empty) rgt =
    let
        l_end = get_label rgt
        (stmt_head, rgt') = cCond c False l_end $ update_label rgt
        then_stmt = cAStmt s rgt'
    in
        case c of
            Number _ n ->
                if n == 0
                then ([], rgt)
                else let res = cAStmt s rgt in (fst res, snd res)
            _ -> (stmt_head ++ fst then_stmt ++ [l_end ++ ":"], snd then_stmt)

cAStmt (IfStmt _ c s es) rgt =
    let
        l_else = get_label rgt
        l_end = get_label $ snd else_stmt

        (stmt_head, rgt') = cCond c False l_else $ update_label $ snd else_stmt

        then_stmt = cAStmt s  (update_label rgt)
        else_stmt = cAStmt es (snd then_stmt)
    in
        case c of
            Number _ n ->
                if n == 0
                then let res = cAStmt es rgt in (fst res, snd res)
                else let res = cAStmt  s rgt in (fst res, snd res)
            _ -> (stmt_head ++ fst then_stmt ++ ["\tjmp\t" ++ l_end] ++
                 [l_else ++ ":"] ++ fst else_stmt ++ [l_end ++ ":"], rgt')

cAStmt (DoStmt _ lb cnd) rgt =
    let
        l_beg = get_label rgt

        rgt' = update_label rgt
        l_continue = get_label rgt'

        rgt'' = update_label rgt'
        l_break = get_label rgt''

        body = cAStmt lb
            $ Map.insert ".continue" (l_continue, 0)
            $ Map.insert ".break" (l_break, 0) $ update_label rgt''

        (cmpr, rgt_final) = cCond cnd True l_beg $ snd body
        cond_inst = case cnd of
            Number _ n | n == 0 -> []
                       | n /= 0 -> ["\tjmp\t" ++ l_beg]
            _ -> cmpr
    in
        ([l_beg ++ ":"] ++ fst body ++
         [l_continue ++ ":"] ++ cond_inst ++
         [l_break ++ ":"]
        , rgt_final)

cAStmt (ForStmt _ init cond step lpbd) rgt =
    let
        l_body = get_label rgt

        rgt' = update_label rgt
        l_continue = get_label rgt'

        rgt'' = update_label rgt'
        l_cond = get_label rgt''

        rgt''' = update_label rgt''
        l_break = get_label rgt'''

        init_stmt = cAStmt init $ update_label rgt'''
        step_stmt = cAStmt step $ snd init_stmt
        loop_body = cAStmt lpbd
            $ Map.insert ".continue" (l_continue, 0)
            $ Map.insert ".break" (l_break, 0) $ snd step_stmt
        (cond_expr, rgt_final) = cCond cond True l_body (snd loop_body) -- expr,, can be empty

        cond_inst = case cond of
            Empty -> ["\tjmp\t" ++ l_body]
            Number _ n | n == 0 -> []
                       | n /= 0 -> ["\tjmp\t" ++ l_body]
            _ -> cond_expr
    in
        (fst init_stmt ++ ["\tjmp\t" ++ l_cond] ++
         [l_body ++ ":"] ++ fst loop_body ++
         [l_continue ++ ":"] ++ fst step_stmt ++
         [l_cond ++ ":"] ++ cond_inst ++
         [l_break ++ ":"]
        , rgt_final)

cAStmt (Break _) rgt = case Map.lookup ".break" rgt of
    Just (a, _) -> (["\tjmp\t" ++ a], rgt)

cAStmt (Continue _) rgt = case Map.lookup ".continue" rgt of
    Just (a, _) -> (["\tjmp\t" ++ a], rgt)

cAStmt (Assign _ (Identifier _ i) e) rgt =
    let
        (expr, reg, rgt') = cExpr e rgt
        (regl, siz) = (\(Just x)->x) $ Map.lookup i rgt
        inst = case e of
            Number _ n -> ["\tmovq\t$" ++ show n ++ ", " ++ regl]
            _          -> if isRegGroup reg
                          then expr ++ conn_inst "movq" reg "%rax" ++ conn_inst "movq" "%rax" regl
                          else expr ++ conn_inst "movq" reg regl
    in
        (inst, rgt')

cAStmt (Assign _ (Array _ (Identifier _ i) ei) ev) rgt =
    let
        (expri, regi, rgt')  = cExpr ei rgt
        (exprv, regv, rgt'') = cExpr ev rgt'

        regv' = if isReg regv && not (isRegGroup regv) then get_high_reg regv else regv

        (nam, siz) = (\(Just x) -> x) $ Map.lookup i rgt

        calc_offset = case ei of
            Number _ n -> conn_inst "movq" ('$' : show (n * 8)) "%rax"
            _ -> expri ++ (if regi == "%rax" then [] else conn_inst "movq" regi "%rax")
                       ++ (conn_inst "imulq" "$8" "%rax")

        calc_addr = conn_inst "leaq" nam "%rbx"         -- rbx = addr base
                     ++ conn_inst "addq" "%rax" "%rbx"  -- rbx = rbx + addr offset

        inst_mov = \x -> conn_inst "movq" x "(%rbx)"
    in
        (case ev of
            Number _ n -> calc_offset ++ calc_addr ++ inst_mov ('$' : show n)
            _          -> exprv
                          ++ conn_inst_s "pushq" regv'
                          ++ calc_offset
                          ++ calc_addr
                          ++ conn_inst_s "popq" "%rax"
                          ++ inst_mov "%rax"
        , rgt'')

cAStmt (Ret _ e) rgt =
    let
        (expr, reg, rgt') = cExpr e rgt
        mov_inst = case e of
            Empty      -> [clr_reg "%rax"]
            Number _ n -> ["\tmovq\t$" ++ show n ++ ", %rax"]
            _          -> expr ++ (if reg == "%rax" then [] else ["\tmovq\t" ++ reg ++ ", %rax"])
    in
        (mov_inst ++ ["\tjmp\t" ++ get_end_label rgt], rgt')

cAStmt (FuncCall _ (Identifier _ fn) pl) rgt =
    let
        (params_h, params_t) = splitAt 6 pl
        reg_l = zip (tail $ tail $ map (!!0) registers) params_h
        (inst_calc, rgt') = foldl step_t ([], rgt) pl -- calculate all params and push then into stack
        inst_popv = foldr step_h [] reg_l             -- pop first 6 params
        param_cnt = length inst_popv
    in
        ([]
         ++ inst_calc
         ++ inst_popv
         ++ [clr_reg "%rax"]
         ++ conn_inst_s "call" (fn ++ "#" ++ (show param_cnt))
         ++ (if null params_t then [] else ["\taddq\t$" ++ show (8 * length params_t) ++ ", %rsp"]) -- release mem
        , rgt')
    where
        step_t (z, rgtx) x = case x of
            Str _ s    -> let (addr, rgtx') = getStrObjAddr s rgtx
                          in  (conn_inst "leaq" addr "%rax" ++ conn_inst_s "pushq" "%rax" ++ z, rgtx')
            Number _ n -> (["\tpushq\t$" ++ show n] ++ z, rgtx)
            e          -> let (expr, reg, rgtx') = cExpr e rgtx
                          in  (expr ++ ["\tpushq\t" ++ get_high_reg reg] ++ z, rgtx')
        step_h (r, e) z = case e of
            Empty -> z
            _     -> ["\tpopq\t" ++ get_high_reg r] ++ z

cAStmt (Rd _ xs) rgt = (\(a, b) -> (b, a)) $ foldr step (rgt, []) xs
    where
        step (Identifier _ x) (rgt, zero) =
            let (reg, addr, rgt') =
                    case Map.lookup x rgt of
                        Just (r, 1) -> let (addr, rgt') = getStrObjAddr "%c" rgt
                                       in  (r, addr, rgt')
                        Just (r, 8) -> let (addr, rgt') = getStrObjAddr "%lld" rgt
                                       in  (r, addr, rgt')
            in  (,) rgt' $ conn_inst "leaq" addr "%rdi" ++ conn_inst "movq" "$0" "%rax"
                        ++ conn_inst_s "call" "read#1"
                        ++ conn_inst "movq" "%rax`fc" (fst $ getItem x rgt')
                        ++ zero

cAStmt (Wt _ (Str _ s) es) rgt = cAStmt (FuncCall (0,0) (Identifier (0,0) "print") ((pStr s) : es)) rgt

-- return: (result, register where result is, updated register table)
cExpr :: Ast -> RegTable -> ([String], String, RegTable)
cExpr (BinNode op _ l r) rgt =
    let
        regr'  = get_free_reg [regr, regl]
        regl'  = get_free_reg [regr, regr']
        hregl  = get_high_reg regl
        hregl' = get_high_reg regl'
        rgt_final = if op `elem` [And, Or] then (update_label . update_label) rgt'' else rgt''
        ret a b c d = (a b c d, d, rgt_final)
    in
        case (exprl, exprr) of
            ([], []) -> if op `elem` [And, Or]
                        then ret bind op regl regr'
                        else (conn_inst "movq" regr regr' ++ bind op regl regr', reg_ret regr', rgt_final) -- n op n
            (_ , []) -> if op `elem` [And, Or]
                        then ret bind op regl regr'
                        else (exprl ++ conn_inst "movq" regr regr' ++ bind op regl regr', reg_ret regr', rgt_final) -- n op l
            ([], _ ) -> if op `elem` [And, Or]
                        then ret bind op regl regr
                        else (exprr ++ bind op regl regr, reg_ret regr, rgt_final) -- r op n
            (_ , _ ) -> if op `elem` [And, Or]
                        then ret bind op regl regr
                        else (exprl ++ ["\tpushq\t" ++ hregl] ++ -- r op l
                              exprr ++ ["\tpopq\t" ++ hregl'] ++ bind op regl' regr, reg_ret regr, rgt_final)
    where
        (exprl, regl, rgt') = case r of
            Number _ a -> ([], "$" ++ show a, rgt)
            _ -> cExpr r rgt
        (exprr, regr, rgt'') = cExpr l rgt'

        bind Gt  l r = conn_inst "cmpq"  l r ++ set_reg "g"  r
        bind Ls  l r = conn_inst "cmpq"  l r ++ set_reg "l"  r
        bind GE  l r = conn_inst "cmpq"  l r ++ set_reg "ge" r
        bind LE  l r = conn_inst "cmpq"  l r ++ set_reg "le" r
        bind Equ l r = conn_inst "cmpq"  l r ++ set_reg "e"  r
        bind Neq l r = conn_inst "cmpq"  l r ++ set_reg "ne" r
        bind Add l r = conn_inst "addq"  l r
        bind Sub l r = conn_inst "subq"  l r
        bind Mul l r = conn_inst "imulq" l r
        bind Div l r = let l' = get_free_reg [l, r, "%rax"] in
           if r == "%rax" then
               ["\tcltd", "\tidivq\t" ++ l]
           else
               if l == "%rax" then
                   conn_inst "movq" l l' ++ conn_inst "movq" r "%rax" ++ ["\tcqto", "\tidivq\t" ++ l']
               else
                   conn_inst "movq" r "%rax" ++ ["\tcltd", "\tidivq\t" ++ l]
        bind Or  l r =
            let
                l1 = get_label rgt''
                l2 = get_label $ update_label rgt''
                jp = ["\tjne\t" ++ l1]
                fix_reg reg = if head reg /= '%'
                              then conn_inst "movq" reg r ++ conn_inst "testq" r r ++ jp
                              else conn_inst "testq" reg reg ++ jp
                fall_back = conn_inst "movq" "$0" r ++ ["\tjmp\t" ++ l2] ++
                            [l1 ++ ":"] ++ conn_inst "movq" "$1" r ++ [l2 ++ ":"]
            in
                exprr ++ fix_reg regr ++
                exprl ++ fix_reg regl ++
                fall_back

        bind And l r =
            let
                l1 = get_label rgt''
                l2 = get_label $ update_label rgt''
                jp = ["\tje\t" ++ l1]
                fix_reg reg = if head reg /= '%'
                              then conn_inst "movq" reg r ++ conn_inst "testq" r r ++ jp
                              else conn_inst "testq" reg reg ++ jp
                fall_back = conn_inst "movq" "$1" r ++ ["\tjmp\t" ++ l2] ++
                            [l1 ++ ":"] ++ conn_inst "movq" "$0" r ++ [l2 ++ ":"]
            in
                exprr ++ fix_reg regr ++
                exprl ++ fix_reg regl ++
                fall_back

        reg_ret r = case op of
            Div -> "%rax"
            _   -> r
        set_reg i reg = let low = get_low_reg reg in
            ["\tset" ++ i ++ "\t" ++ low] ++ conn_inst "movzbq" low reg

cExpr (UnaryNode Not _ e) rgt =
    let
        (expr, reg, rgt') = cExpr e rgt
    in
        (["\tcmpq\t$0, " ++ reg, "\tsete\t%al", "\tmovzbq\t%al, %rax"], "%rax", rgt')

cExpr (UnaryNode Neg _ e) rgt =
    let
        (expr, reg, rgt') = cExpr e rgt
    in
        if last reg == ')'
        then (["\tmovq\t" ++ reg ++ ", %rax", "\tnegq\t%rax"], "%rax", rgt')
        else (["\tnegq\t" ++ reg], reg, rgt')

cExpr (Number _ n) rgt = (["\tmovq\t$" ++ show n ++ ", %rax"], "%rax", rgt)

cExpr (Array _ (Identifier _ i) e) rgt =
    let
        (expr, reg, rgt') = cExpr e rgt

        mov_inst = case e of
            Number _ n -> ["\tmovq\t$" ++ show n ++ ", %rax"]
            _ -> expr ++ (if reg == "%rax" then [] else ["\tmovq\t" ++ reg ++ ", %rax"])

        regf = get_free_reg ["%rax", "%rbx", reg]
        regfl = get_low_reg regf

        (r, s) = (\(Just x) -> x) $ Map.lookup i rgt

        move_to_free r = ["\tmovq\t" ++ r ++ ", " ++ regf]
    in
        (mov_inst -- rax = index
          ++ conn_inst "imulq" "$8" "%rax"                          -- rax = addr offset
          ++ conn_inst "leaq" r "%rbx"                              -- rbx = addr base
          ++ conn_inst "addq" "%rax" "%rbx"                         -- rbx = rbx + rax
          ++ move_to_free "(%rbx)"                                  -- free reg = *rax
        , regf, rgt')

cExpr (Identifier _ i) rgt = case Map.lookup i rgt of
    Just (a, _) -> ([], a, rgt)
    -- Just (a, 1) -> (["\tmovzbl\t" ++ a ++ ", %eax", "\tmovsbl\t%al, %eax"], "%eax", rgt)

cExpr fc@(FuncCall _ _ _) rgt = let inst = cAStmt fc rgt in
    (fst inst, "%rax`fc", snd inst)

-- Note: for-stmt, ret-stmt and write-stmt have empty-expr
--       return %eax just for ret-stmt
cExpr Empty rgt = ([], "%rax", rgt)

cExpr (Ch _ c) rgt = cExpr (Number (0,0) (ord c)) rgt


-- exp -> default-jump-method -> jump-target -> register-table -> (asm, register-table)
cCond :: Ast -> Bool -> String -> RegTable -> ([String], RegTable)
cCond exp fb_j lb rgt = case exp of
    BinNode op _ l r | op `elem` [Gt, Ls, GE, LE, Equ, Neq] ->
        let
            (exprl, regl, rgt' ) = cExpr r rgt
            (exprr, regr, rgt'') = cExpr l rgt'

            regr'  = get_free_reg [regr, regl]
            regl'  = get_free_reg [regr, regr']
            hregl  = get_high_reg regl
            hregl' = get_high_reg regl'
        in case (exprl, exprr) of
            ([], []) -> (conn_inst "movq" regr regr' ++ bind op regl regr', rgt'') -- n op n
            (_ , []) -> (exprl ++ conn_inst "movq" regr regr' ++ bind op regl regr', rgt'') -- n op l
            ([], _ ) -> (exprr ++ bind op regl regr, rgt'') -- r op n
            (_ , _ ) -> (exprl ++ ["\tpushq\t" ++ hregl] ++ -- r op l
                         exprr ++ ["\tpopq\t" ++ hregl'] ++ bind op regl' regr, rgt'')
    _ ->
        let (exprx, regx, rgtx) = cExpr exp rgt in
            (exprx ++ conn_inst "cmpq" "$0" regx ++ jmp "ne", rgtx)
    where
        bind Gt  l r = conn_inst "cmpq"  l r ++ jmp "g"
        bind Ls  l r = conn_inst "cmpq"  l r ++ jmp "l"
        bind GE  l r = conn_inst "cmpq"  l r ++ jmp "ge"
        bind LE  l r = conn_inst "cmpq"  l r ++ jmp "le"
        bind Equ l r = conn_inst "cmpq"  l r ++ jmp "e"
        bind Neq l r = conn_inst "cmpq"  l r ++ jmp "ne"

        jmp i = ["\tj" ++ fix_md i ++ "\t" ++ lb]

        fix_md x = if fb_j then x else fix_md' x
        fix_md' "g"  = "le"
        fix_md' "l"  = "ge"
        fix_md' "ge" = "l"
        fix_md' "le" = "g"
        fix_md' "e"  = "ne"
        fix_md' "ne" = "e"


getStrObjAddr str sgt =
    let (_, idx) = getItem ".LC" sgt
        lcn = ".LC" ++ show idx ++ "(%rip)"
    in  case Map.lookup (':':str) sgt of
        Nothing -> (lcn, Map.insert ".LC" ("", idx + 1) $ Map.insert (':':str) (lcn, 0) sgt)
        Just  x -> (fst x, sgt)
