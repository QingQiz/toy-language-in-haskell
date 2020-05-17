module CodeGen(runCodeGen) where

import Ast
import Register

import Data.Char
import Data.List
import Data.List.Utils
import qualified Data.Map as Map

runCodeGen = cProgram


cProgram :: Ast -> [String]
cProgram (Program _ _ vds fds) = concat $ bind (cGlobalVarDef vds) (cFuncDefs fds)
    where
        bind (a, b) c = bind' a (c b)
        bind' a (b, c) = [a ++ c, b]


cGlobalVarDef :: [Ast] -> ([String], RegTable)
cGlobalVarDef defs = foldr step ([], empty_rgt) defs where
    step (VarDef t _ xs) (l, rgt) =
        let
            rst = case t of
                TInt  -> foldr (step' 4) ([], empty_rgt) xs
                TChar -> foldr (step' 1) ([], empty_rgt) xs
        in
            (fst rst ++ l, Map.union (snd rst) rgt)

    step' s (Identifier _ i) (l, rgt) =
        (("\t.comm\t" ++ i ++ "," ++ show s) : l, Map.insert i (i ++ "(%rip)" , s) rgt)
    step' s (Array _ (Identifier _ i) (Number _ n)) (l, rgt) =
        (("\t.comm\t" ++ i ++ "," ++ show (n * s)) : l, Map.insert i (i ++ "(%rip)", s) rgt)

--                                 code      all_func_name
cFuncDefs :: [Ast] -> RegTable -> ([String], [String])
cFuncDefs fds rgt = (concat $ foldr (\fd zero -> cAFuncDef fd rgt : zero) [] fds, foldr collect_fn [] fds)
    where
        collect_fn (FuncDef _ _ (Identifier _ fn) _ _) z = ("\t.globl\t" ++ fn) : z


cAFuncDef :: Ast -> RegTable -> [String]
cAFuncDef (FuncDef ft _ (Identifier _ fn) pl fb) rgt =
    [fn ++ ":", "\tpushq\t%rbp", "\tmovq\t%rsp, %rbp"]
        ++ bind (cParams pl) (cComdStmt fb)
        ++ ["\tret"]
    where
        -- clear %eax if function does not have a return-stmt
        label_end = [clr_reg "%eax", ".L_" ++ fn ++ "_END:"]
        bind (a, b) c = bind' a $ c $ Map.insert ".label" (fn, 1) $ Map.union b rgt
        bind' a (b, c) = if c /= 0
                         then ["\tsubq\t$" ++ show c ++ ", %rsp"] ++ a ++ b ++ label_end ++ ["\tleave"]
                         else a ++ b ++ label_end ++ ["\tpopq\t%rbp"]

        cParams pls = (for_pl pls, Map.fromList $ get_param_reg pls)

        get_param_reg pls =
            let l2 = unzip $ map (\(t, Identifier _ i) -> ((\t -> case t of TInt -> 4; TChar -> 1) t, i)) pls
                regs = map (\a -> show a ++ "(%rbp)") $ [-4,-8..(-24)] ++ [16,24..]
            in zip (snd l2) $ zip regs $ fst l2 -- (name, (reg, size))

        for_pl pls =
            let pl = get_param_reg pls
                si = unzip $ map (\(_,(_,c))->case c of 4->(1,"movl"); 1->(3,"movb")) pl
                reg = map (\inp->(registers!!inp)!!(fst si!!(inp-1))) [1..6]
            in map (\(a,b,c)->"\t" ++ a ++ "\t" ++ b ++ ", " ++ c) $ zip3 (snd si) reg $ map (\(_,(b,_))->b) pl


cComdStmt :: Ast -> RegTable -> ([String], Int) -- Int: stack size needed
cComdStmt (ComdStmt _ [] vd sl) rgt =
    let (var, siz) = cLocalVar vd rgt in (fst $ cStmtList sl var, siz)


cLocalVar :: [Ast] -> RegTable -> (RegTable, Int) -- Int: stack size
cLocalVar vds rgt =
    let
        offset :: [Int]
        offset = map (get_reg_offset . head)
            $ filter (not . null)
            $ map (fst . span ("(%rbp)" `isSuffixOf`) . tails)
            $ map (\(_, (b,_))->b) $ Map.toList rgt

        int_var = map trans $ concat $ map take_name [x | x <- vds, is_int x]
        chr_var = map trans $ concat $ map take_name [x | x <- vds, not $ is_int x]

        int_rgt = for_siz 4 int_var (if null offset then 0 else minimum offset) []
        chr_rgt = for_siz 1 chr_var (fst int_rgt) []
    in
        (Map.union (Map.union (snd int_rgt) (snd chr_rgt)) rgt, (abs $ fst chr_rgt - 15) `div` 16 * 16)
    where
        is_int (VarDef TInt _ _)  = True
        is_int (VarDef TChar _ _) = False

        trans (Array _ (Identifier _ i) (Number _ n)) = (i, n)
        trans (Identifier _ i) = (i, 1)

        take_name = \(VarDef _ _ n) -> n

        for_siz siz ((name, len):xs) oft res = for_siz siz xs (oft - siz - len * siz)
            $ (name, ((show $ oft - len * siz) ++ "(%rbp)", siz)) : res
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
        (stmt_head, reg) = cExpr c rgt
        stmt_cmpr = ["\tcmpl\t$0, " ++ reg, "\tje\t" ++ l_end]
        then_stmt = cAStmt s (update_label rgt)
    in
        (stmt_head ++ stmt_cmpr ++ fst then_stmt ++ [l_end ++ ":"], snd then_stmt)

cAStmt (IfStmt _ c s es) rgt =
    let
        l_else = get_label rgt
        l_end = get_label $ snd else_stmt

        (stmt_head, reg) = cExpr c rgt
        stmt_cmpr = ["\tcmpl\t$0, " ++ reg, "\tje\t" ++ l_else]

        then_stmt = cAStmt s  (update_label rgt)
        else_stmt = cAStmt es (snd then_stmt)
    in
        (stmt_head ++ stmt_cmpr ++ fst then_stmt ++ ["\tjmp\t" ++ l_end]
         ++ [l_else ++ ":"] ++ fst else_stmt ++ [l_end ++ ":"], update_label $ snd else_stmt)

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

        (cmpr, reg) = cExpr cnd rgt''
        cond_inst = case cnd of
            Number _ n | n == 0 -> []
                       | n /= 0 -> ["\tjmp\t" ++ l_beg]
            _ -> cmpr ++ ["\tcmpl\t$0, " ++ reg, "\tjne\t" ++ l_beg]
    in
        ([l_beg ++ ":"] ++ fst body ++
         [l_continue ++ ":"] ++ cond_inst ++
         [l_break ++ ":"]
        , snd body)

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
        (cond_expr, reg) = cExpr cond rgt -- expr,, can be empty

        cond_inst = case cond of
            Empty -> ["\tjmp\t" ++ l_body]
            Number _ n | n == 0 -> []
                       | n /= 0 -> ["\tjmp\t" ++ l_body]
            _ -> cond_expr ++ ["\tcmpl\t$0, " ++ reg, "\tjne\t" ++ l_body]
    in
        (fst init_stmt ++ ["\tjmp\t" ++ l_cond] ++
         [l_body ++ ":"] ++ fst loop_body ++
         [l_continue ++ ":"] ++ fst step_stmt ++
         [l_cond ++ ":"] ++ cond_inst ++
         [l_break ++ ":"]
        , snd loop_body)

cAStmt (Break _) rgt = case Map.lookup ".break" rgt of
    Just (a, _) -> (["\tjmp\t" ++ a], rgt)

cAStmt (Continue _) rgt = case Map.lookup ".continue" rgt of
    Just (a, _) -> (["\tjmp\t" ++ a], rgt)

cAStmt (Assign _ (Identifier _ i) e) rgt =
    let
        (expr, reg) = cExpr e rgt
        (regl, siz) = (\(Just x)->x) $ Map.lookup i rgt
        inst = case (e, siz) of
            (Number _ n, 1) -> "\tmovb\t$" ++ show n ++ ", " ++ regl
            (Number _ n, 4) -> "\tmovl\t$" ++ show n ++ ", " ++ regl
            (_         , 1) -> "\tmovb\t" ++ get_low_reg reg ++ ", " ++ regl
            (_         , 4) -> "\tmovl\t" ++ reg ++ ", " ++ regl
    in
        (expr ++ [inst], rgt)

cAStmt (Assign _ (Array _ (Identifier _ i) ei) ev) rgt =
    let
        (expri, regi) = cExpr ei rgt
        (exprv, regv) = cExpr ev rgt

        ins_cltq = case ei of
            Number _ n -> ["\tmovl\t$" ++ show n ++ ", %eax", "\tcltq"]
            _ -> if regi == "%eax"
                 then ["\tcltq"]
                 else expri ++ ["\tmovl\t" ++ regi ++ ", %eax", "\tcltq"]

        push_regv = if last regv == ')'
                    then ["\tpushq\t" ++ regv]
                    else ["\tpushq\t" ++ get_high_reg regv]
        (nam,siz) = (\(Just x) -> x) $ Map.lookup i rgt

        instn a b = case siz of
            1 -> "\tmovb\t$" ++ show a ++ ", " ++ b
            4 -> "\tmovl\t$" ++ show a ++ ", " ++ b
        inst  a b = case siz of
            1 -> "\tmovb\t" ++ get_low_reg a ++ ", " ++ b
            4 -> "\tmovl\t" ++ a ++ ", " ++ b
    in
        (if "(%rip)" `isSuffixOf` nam
        then case ev of
            Number _ n ->
                ins_cltq ++                                      -- calculate index and save it in %eax
                ["\tleaq\t0(,%rax," ++ show siz ++ "), %rdx"] ++ -- %rdx is the offset of array now
                ["\tleaq\t" ++ nam ++ ", %rax"] ++
                [instn n "(%rdx,%rax)"]
            _ ->
                exprv ++ push_regv ++                            -- calculate value and push it into stack
                ins_cltq ++                                      -- calculate index and save it in %eax
                ["\tpopq\t%rcx"] ++                              -- %ecx is now value
                ["\tleaq\t0(,%rax," ++ show siz ++ "), %rdx"] ++ -- %rdx is the offset of array now
                ["\tleaq\t" ++ nam ++ ", %rax"] ++
                [inst "%ecx" "(%rdx,%rax)"]
        else case ev of
            Number _ n ->
                ins_cltq ++           -- calculate index and save it in %eax
                [instn n $ init nam ++ ",%rax," ++ show siz ++ ")"]
            _ ->
                exprv ++ push_regv ++ -- calculate value and push it into stack
                ins_cltq ++           -- calculate index and save it in %eax
                ["\tpopq\t%rdx"] ++   -- %edx is the value
                [inst "%edx" $ init nam ++ ",%rax," ++ show siz ++ ")"]
        , rgt)

cAStmt (Ret _ e) rgt =
    let
        (expr, reg) = cExpr e rgt
        mov_inst = case e of
            Empty      -> [clr_reg "%eax"]
            Number _ n -> ["\tmovl\t$" ++ show n ++ ", %eax"]
            _          -> expr ++ (if reg == "%eax" then [] else ["\tmovl\t" ++ reg ++ ", %eax"])
    in
        (mov_inst ++ ["\tjmp\t" ++ get_end_label rgt], rgt)

cAStmt (FuncCall _ (Identifier _ fn) pl) rgt =
    let
        (params_h, params_t) = splitAt 6 pl
        reg_l = zip (tail $ map (!!1) registers) params_h
    in
        ((foldl step_t [] pl) ++ -- calculate all params and push then into stack
         (foldr step_h [] reg_l) ++ -- pop first 6 params
         [clr_reg "%eax", "\tcall\t" ++ fn] ++
         (if null params_t then [] else ["\taddq\t$" ++ show (8 * length params_t) ++ ", %rsp"]) -- release mem
        , rgt)
    where
        step_t z x = case x of
            Empty      -> z
            Number _ n -> ["\tpushq\t$" ++ show n] ++ z
            e          -> let (expr, reg) = cExpr e rgt in
                expr ++ ["\tpushq\t" ++ get_high_reg reg] ++ z
        step_h (r, e) z = case e of
            Empty -> z
            _     -> ["\tpopq\t" ++ get_high_reg r] ++ z

cAStmt (Rd _ xs) rgt = (foldr step [] xs, rgt)
    where
        step (Identifier _ x) zero =
            let (reg, format_str) = case Map.lookup x rgt of
                    Just (r, 1) -> (r, "$25381") -- "%c\0"
                    Just (r, 4) -> (r, "$25637") -- "%d\0"
            in ["\tpushq\t$0",
                "\tpushq\t" ++ format_str,
                "\tleaq\t" ++ reg ++ ", %rdx",
                "\tleaq\t(%rsp), %rax",
                "\tmovq\t%rdx, %rsi",
                "\tmovq\t%rax, %rdi",
                clr_reg "%eax",
                "\tcall\t__isoc99_scanf@PLT",
                "\taddq\t$16, %rsp"] ++ zero

cAStmt (Wt _ (Str _ s) es) rgt =
    let
        format_str = convert_str $ foldr (\x z -> replace (fst x) (snd x) z) s spcChr

        (func_call, _) = cAStmt (FuncCall (0,0) (Identifier (0,0) "printf@PLT") (Empty : es)) rgt
        (a, b) =
            if "%rsp" `isInfixOf` (last func_call)
            then (fst $ splitAt (length func_call - 3) func_call, [(last . init) func_call, last func_call])
            else (fst $ splitAt (length func_call - 2) func_call, [last func_call])
        siz = if length es - 5 > 0 then show ((length es - 5) * 8) else ""

        spcChr = [("\\b", "\b"), ("\\t", "\t"), ("\\n", "\n"), ("\\r", "\r")]
    in
        (["\tpushq\t$0"] ++
         foldr (\x z -> ["\tmovabsq\t$" ++ x ++ ", %rax", "\tpushq\t%rax"] ++ z) [] format_str ++
         a ++ ["\tleaq\t" ++ siz ++ "(%rsp), %rdi", clr_reg "%eax"] ++ b ++
         ["\taddq\t$" ++ show (8 + 8 * length format_str) ++ ", %rsp"]
        , rgt)
    where
        convert_str s = reverse $ map (show . from_bin . foldr (\b a -> show b ++ a) "" . concat . reverse)
            $ splitEvery 8
            $ map (\inp -> take (8 - length inp) [0,0..] ++ inp)
            $ map (to_bin . ord) s

        to_bin = reverse . unfoldr (\x -> if x == 0 then Nothing else Just (x `mod` 2, x `div` 2))
        from_bin = foldl (\zero x -> zero * 2 + (fromEnum $ x == '1')) 0

        splitEvery _ [] = []
        splitEvery n xs = a : splitEvery n b where
            (a, b) = splitAt n xs


--                             res     reg where res is
cExpr :: Ast -> RegTable -> ([String], String)
cExpr (BinNode op _ l r) rgt =
    let
        (exprl, regl) = case r of
            Number _ a -> ([], "$" ++ show a)
            _ -> cExpr r rgt
        (exprr, regr) = cExpr l rgt
        regr'  = get_free_reg [regr, regl]
        regl'  = get_free_reg [regr, regr']
        hregl  = get_high_reg regl
        hregl' = get_high_reg regl'

    in
        case (exprl, exprr) of
            ([], []) -> (conn_inst "movl" regr regr' ++ bind op regl regr', reg_ret regr') -- n op n
            ([], _ ) -> (exprr ++ bind op regl regr, reg_ret regr) -- r op n
            (_ , []) -> (exprl ++ conn_inst "movl" regr regr' ++ bind op regl regr', reg_ret regr') -- n op l
            (_ , _ ) -> (exprl ++ ["\tpushq\t" ++ hregl] ++ -- r op l
                         exprr ++ ["\tpopq\t" ++ hregl'] ++ bind op regl' regr, reg_ret regr)
    where
        bind Gt  l r = conn_inst "cmpl"  l r ++ set_reg "g"  r
        bind Ls  l r = conn_inst "cmpl"  l r ++ set_reg "l"  r
        bind GE  l r = conn_inst "cmpl"  l r ++ set_reg "ge" r
        bind LE  l r = conn_inst "cmpl"  l r ++ set_reg "le" r
        bind Equ l r = conn_inst "cmpl"  l r ++ set_reg "e"  r
        bind Neq l r = conn_inst "cmpl"  l r ++ set_reg "ne" r
        bind Or  l r = conn_inst "orl"   l r ++ conn_inst "cmpl" "$0" r ++ set_reg "ne" r
        bind And l r = bind Mul          l r ++ conn_inst "cmpl" "$0" r ++ set_reg "ne" r
        bind Add l r = conn_inst "addl"  l r
        bind Sub l r = conn_inst "subl"  l r
        bind Mul l r = conn_inst "imull" l r
        bind Div l r = let l' = get_free_reg [l, r, "%eax"] in
           if r == "%eax" then
               ["\tcltd", "\tidivl\t" ++ l]
           else
               if l == "%eax" then
                   conn_inst "movl" l l' ++ conn_inst "movl" r "%eax" ++ ["\tcltd", "\tidivl\t" ++ l']
               else
                   conn_inst "movl" r "%eax" ++ ["\tcltd", "\tidivl\t" ++ l]
        reg_ret r = case op of
            Div -> "%eax"
            _   -> r
        conn_inst cmd l r = ["\t" ++ cmd ++ "\t" ++ l ++ ", " ++ r]
        set_reg i reg = let low = get_low_reg reg in
            ["\tset" ++ i ++ "\t" ++ low] ++ conn_inst "movzbl" low reg

cExpr (UnaryNode Not _ e) rgt =
    let
        (expr, reg) = cExpr e rgt
    in
        (["\tcmpl\t$0, " ++ reg, "\tsete\t%al", "\tmovzbl\t%al, %eax"], "%eax")

cExpr (UnaryNode Neg _ e) rgt =
    let
        (expr, reg) = cExpr e rgt
    in
        if last reg == ')'
        then (["\tmovl\t" ++ reg ++ ", %eax", "\tnegl\t%eax"], "%eax")
        else (["\tnegl\t" ++ reg], reg)

cExpr (Number _ n) rgt = (["\tmovl\t$" ++ show n ++ ", %eax"], "%eax")

cExpr (Array _ (Identifier _ i) e) rgt =
    let
        (expr, reg) = cExpr e rgt

        mov_inst = case e of
            Number _ n -> ["\tmovl\t$" ++ show n ++ ", %eax", "\tcltq"]
            _ -> expr ++ (if reg == "%eax" then [] else ["\tmovl\t" ++ reg ++ ", %eax"]) ++ ["\tcltq"]

        regf = get_free_reg ["%eax", "%edx", reg]
        regfl = get_low_reg regf

        (r, s) = (\(Just x) -> x) $ Map.lookup i rgt

        move_to_free r = if s == 4
                         then ["\tmovl\t" ++ r ++ ", " ++ regf]
                         else ["\tmovzbl\t" ++ r ++ ", " ++ regf, "\tmovsbl\t" ++ regfl ++ "," ++ regf]
    in
        if "(%rip)" `isSuffixOf` r
        then
            (mov_inst ++ -- calculate index and save it in %eax
             ["\tleaq\t0(,%rax," ++ show s ++ "), %rdx"] ++ -- offset
             ["\tleaq\t" ++ r ++ ", %rax"] ++ -- addr
             move_to_free "(%rdx,%rax)"
            , regf)
        else
            (mov_inst ++ move_to_free (init r ++ ",%rax," ++ show s ++ ")"), regf)

cExpr (Identifier _ i) rgt = case Map.lookup i rgt of
    Just (a, 4) -> ([], a)
    Just (a, 1) -> (["\tmovzbl\t" ++ a ++ ", %eax", "\tmovsbl\t%al, %eax"], "%eax")

cExpr fc@(FuncCall _ _ _) rgt = (fst $ cAStmt fc rgt, "%eax")

-- Note: for-stmt, ret-stmt and write-stmt have empty-expr
--       return %eax just for ret-stmt
cExpr Empty _ = ([], "%eax")

cExpr (Ch _ c) rgt = cExpr (Number (0,0) (ord c)) rgt


