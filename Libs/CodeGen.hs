module CodeGen where

import Ast
import Symbol
import Register

import Data.Char
import Data.List
import Data.List.Utils
import qualified Data.Map as Map


cProgram :: Ast -> [String]
cProgram (Program _ vds fds) = concat $ bind (cGlobalVarDef vds) (cFuncDefs fds)
    where
        bind (a, b) c = bind' a (c b)
        bind' a (b, c) = [a ++ c, b]


cGlobalVarDef :: [Ast] -> ([String], RegTable)
cGlobalVarDef defs = foldr step ([], empty_rgt) defs where
    step (VarDef t xs) (l, rgt) =
        let
            rst = case t of
                TInt  -> foldr (step' 4) ([], empty_rgt) xs
                TChar -> foldr (step' 1) ([], empty_rgt) xs
        in
            (fst rst ++ l, Map.union (snd rst) rgt)

    step' s (Identifier i) (l, rgt) =
        (("\t.comm\t" ++ i ++ "," ++ show s) : l, Map.insert i (i ++ "(%rip)" , s) rgt)
    step' s (Array (Identifier i) (Number n)) (l, rgt) =
        (("\t.comm\t" ++ i ++ "," ++ show (n * s)) : l, Map.insert i (i ++ "(%rip)", s) rgt)

--                                 code      all_func_name
cFuncDefs :: [Ast] -> RegTable -> ([String], [String])
cFuncDefs fds rgt = (concat $ foldr (\fd zero -> cAFuncDef fd rgt : zero) [] fds, foldr collect_fn [] fds)
    where
        collect_fn (FuncDef _ (Identifier fn) _ _) z = ("\t.globl\t" ++ fn) : z


cAFuncDef :: Ast -> RegTable -> [String]
cAFuncDef (FuncDef ft (Identifier fn) pl fb) rgt =
    [fn ++ ":", "\tpushq\t%rbp", "\tmovq\t%rsp, %rbp"]
        ++ bind (cParams pl) (cComdStmt fb)
        ++ ["\tret"]
    where
        -- clear %eax if function does not have a return-stmt
        label_end = ["\tmovl\t$0, %eax", ".L_" ++ fn ++ "_END:"]
        bind (a, b) c = bind' a $ c $ Map.insert ".label" (fn, 1) $ Map.union b rgt
        bind' a (b, c) = if c /= 0
                         then ["\tsubq\t$" ++ show c ++ ", %rsp"] ++ a ++ b ++ label_end ++ ["\tleave"]
                         else a ++ b ++ label_end ++ ["\tpopq\t%rbp"]

        cParams pls = (for_pl pls, Map.fromList $ get_param_reg pls)

        get_param_reg pls =
            let l2 = unzip $ map (\(t, Identifier i) -> ((\t -> case t of TInt -> 4; TChar -> 1) t, i)) pls
                regs = map (\a -> show a ++ "(%rbp)") $ [-4,-8..(-24)] ++ [16,32..]
            in zip (snd l2) $ zip regs $ fst l2 -- (name, (reg, size))

        for_pl pls =
            let pl = get_param_reg pls
                si = unzip $ map (\(_,(_,c))->case c of 4->(1,"movl"); 1->(3,"movb")) pl
                reg = map (\inp->(registers!!inp)!!(fst si!!(inp-1))) [1..6]
            in map (\(a,b,c)->"\t" ++ a ++ "\t" ++ b ++ ", " ++ c) $ zip3 (snd si) reg $ map (\(_,(b,_))->b) pl


cComdStmt :: Ast -> RegTable -> ([String], Int) -- Int: stack size needed
cComdStmt (ComdStmt [] vd sl) rgt =
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
        is_int (VarDef TInt _) = True
        is_int (VarDef TChar _) = False

        trans (Array (Identifier i) (Number n)) = (i, n)
        trans (Identifier i) = (i, 1)

        take_name = \(VarDef _ n) -> n

        for_siz siz ((name, len):xs) oft res = for_siz siz xs (oft - siz - len * siz)
            $ (name, ((show $ oft - len * siz) ++ "(%rbp)", siz)) : res
        for_siz _ [] oft res = (oft, Map.fromList res)


cStmtList :: Ast -> RegTable -> ([String], RegTable)
cStmtList (StmtList sl) rgt = cStmts sl rgt where
    cStmts (sl:sls) rgt = bind (cAStmt sl rgt) (cStmts sls)
    cStmts [] rgt = ([], rgt)

    bind (a, b) c = bind' a (c b)
    bind' a (b, c) = (a ++ b, c)


cAStmt :: Ast -> RegTable -> ([String], RegTable)
cAStmt sl@(StmtList _) rgt = cStmtList sl rgt
cAStmt Empty rgt = (["\tnop"], rgt)

cAStmt (IfStmt c s Empty) rgt =
    let
        l_end = get_label rgt
        (stmt_head, reg) = cExpr c rgt
        stmt_cmpr = ["\tcmpl\t$0, " ++ reg, "\tje\t" ++ l_end]
        then_stmt = cAStmt s (update_label rgt)
    in
        (stmt_head ++ stmt_cmpr ++ fst then_stmt ++ [l_end ++ ":"], snd then_stmt)

cAStmt (IfStmt c s es) rgt =
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

cAStmt (DoStmt lb cnd) rgt =
    let
        l_beg = get_label rgt
        body = cAStmt lb $ update_label rgt
        (cmpr, reg) = cExpr cnd rgt
    in
        ([l_beg ++ ":"] ++ fst body ++ cmpr
         ++ ["\tcmpl\t$0, " ++ reg, "\tjne\t" ++ l_beg], snd body)

cAStmt (ForStmt init cond step lpbd) rgt =
    let
        l_body = get_label rgt
        l_cond = get_label $ snd loop_body
        init_stmt = cAStmt init rgt -- assign
        step_stmt = cAStmt step rgt -- assign
        (cond_expr, reg) = cExpr cond rgt -- expr,, can be empty
        loop_body = cAStmt lpbd $ update_label rgt
    in
        case cond of
            Empty ->
                (fst init_stmt ++ ["\tjmp\t" ++ l_cond] ++
                 [l_body ++ ":"] ++ fst loop_body ++ fst step_stmt ++
                 [l_cond ++ ":"]
                , update_label $ snd loop_body)
            _ ->
                (fst init_stmt ++ ["\tjmp\t" ++ l_cond] ++
                 [l_body ++ ":"] ++ fst loop_body ++ fst step_stmt ++
                 [l_cond ++ ":"] ++ cond_expr ++ ["\tcmpl\t$0, " ++ reg, "\tjne\t" ++ l_body]
                , update_label $ snd loop_body)

cAStmt (Assign (Identifier i) e) rgt =
    let (expr, reg) = cExpr e rgt in
        ((++) expr $ case Map.lookup i rgt of
            Just (addr, siz) -> case siz of
                1 -> ["\tmovb\t" ++ get_low_reg reg ++ ", " ++ addr]
                4 -> ["\tmovl\t" ++ reg ++ ", " ++ addr]
            _ -> error $ "an error occurred on assign-stmt"
        , rgt)

cAStmt (Assign (Array (Identifier i) ei) ev) rgt =
    let
        (expri, regi) = cExpr ei rgt
        (exprv, regv) = cExpr ev rgt
        ins_cltq = if regi == "%eax"
                   then []
                   else ["\tmovl\t" ++ regi ++ ", %eax", "\tcltq"]
        push_regv = if last regv == ')'
                    then ["\tpushq\t" ++ regv]
                    else ["\tpushq\t" ++ get_high_reg regv]

    in
        (case Map.lookup i rgt of
            Just (nam, siz) ->
                if "(%rip)" `isSuffixOf` nam
                then
                    expri ++ ins_cltq ++ ["\tpushq\t%rax"] ++
                    exprv ++ push_regv ++
                    ["\tpopq\t%rcx", "\tpopq\t%rax"] ++
                    ["\tleaq\t0(,%rax," ++ show siz ++ "), %rdx", "\tleaq\t" ++ nam ++ ", %rax"] ++
                    ["\tmovl\t%ecx, (%rdx,%rax)"]
                else
                    expri ++ ins_cltq ++ ["\tpushq\t%rax"] ++
                    exprv ++ push_regv ++
                    ["\tpopq\t%rdx", "\tpopq\t%rax"] ++
                    ["\tmovl\t%edx, " ++ init nam ++ ",%rax," ++ show siz ++ ")"]
            _ -> error $ "an error occurred on assign-array"
        , rgt)

cAStmt (Ret e) rgt =
    let
        (expr, reg) = cExpr e rgt
    in
        if reg == "%eax"
        -- return void also clear %eax
        then (expr ++ ["\tmovl\t%0, %eax", "\tjmp\t" ++ get_end_label rgt], rgt)
        else (expr ++ ["\tmovl\t" ++ reg ++ ", %eax", "\tjmp\t" ++ get_end_label rgt], rgt)

cAStmt (FuncCall (Identifier fn) pl) rgt =
    let
        (params_h, params_t) = splitAt 6 pl
        reg_l = zip (tail $ map (!!1) registers) params_h
    in
        ((foldl step_t [] $ params_t) ++ (foldl step_h [] reg_l) ++ ["\tmovl\t$0, %eax", "\tcall\t" ++ fn]
         ++ (if null params_t then [] else ["\taddq\t" ++ show (8 * length params_t) ++ ", %rsp"])
        , rgt)
    where
        step_t zero x = case x of
            (Number n) -> ["\tpushq\t$" ++ show n] ++ zero
            e -> let (expr, reg) = cExpr e rgt in
                expr ++ ["\tpushq\t" ++ get_high_reg reg] ++ zero
        step_h zero (r, ast) = case ast of
            (Number n) -> ["\tmovl\t$" ++ show n ++ ", " ++ r] ++ zero
            e -> let (expr, reg) = cExpr e rgt in
                expr ++ (if reg == r then [] else ["\tmovl\t" ++ reg ++ ", " ++ r]) ++ zero

cAStmt (Rd xs) rgt = (foldr step [] xs, rgt)
    where
        step (Identifier x) zero =
            let (reg, format_str) = case Map.lookup x rgt of
                    Just (r, 1) -> (r, "$25381") -- "%c\0"
                    Just (r, 4) -> (r, "$25637") -- "%d\0"
                    _ -> error $ "an error occurred on rd-stmt"
            in ["\tpushq\t$0",
                "\tpushq\t" ++ format_str,
                "\tleaq\t" ++ reg ++ ", %rdx",
                "\tleaq\t(%rsp), %rax",
                "\tmovq\t%rdx, %rsi",
                "\tmovq\t%rax, %rdi",
                "\tmovl\t$0, %eax",
                "\tcall\t__isoc99_scanf@PLT",
                "\taddq\t$16, %rsp"] ++ zero

cAStmt (Wt (Str s) es) rgt =
    let
        format_str = convert_str $ foldr (\x z -> replace (fst x) (snd x) z) s spcChr

        (func_call, _) = cAStmt (FuncCall (Identifier "printf@PLT") (Empty : es)) rgt
        (a, b) =
            if "%rsp" `isInfixOf` (last func_call)
            then (fst $ splitAt (length func_call - 4) func_call, [(last . init) func_call, last func_call])
            else (fst $ splitAt (length func_call - 3) func_call, [last func_call])
        siz = if length es - 5 > 0 then show ((length es - 5) * 8) else ""

        spcChr = [("\\b", "\b"), ("\\t", "\t"), ("\\n", "\n"), ("\\r", "\r")]
    in
        (["\tpushq\t$0"] ++
         foldr (\x z -> ["\tmovabsq\t$" ++ x ++ ", %rax", "\tpushq\t%rax"] ++ z) [] format_str ++
         a ++ ["\tleaq\t" ++ siz ++ "(%rsp), %rdi", "\tmovl\t$0, %eax"] ++ b ++
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
cExpr (BinNode op l r) rgt =
    let
        (exprl, regl) = case r of
            Number a -> ([], "$" ++ show a)
            _ -> cExpr r rgt
        (exprr, regr) = cExpr l rgt
        regl' = get_free_reg [regr, regl]
        regr' = get_free_reg [regr, regl, regl', "%eax"]
        hregl  = get_high_reg regl
        hregl' = get_high_reg regl'
    in
        case (exprl, exprr) of
            ([], []) -> (conn_inst "movl" regr regr' ++ bind op regl regr', regr')
            ([], _ ) -> (exprr ++ bind op regl regr, regr)
            (_ , []) -> (exprl ++ conn_inst "movl" regr regr' ++ bind op regl regr', regr')
            (_ , _ ) -> (exprl ++ ["\tpushq\t" ++ hregl] ++
                         exprr ++ ["\tpopq\t" ++ hregl'] ++ bind op regl' regr, regr)
    where
         bind Gt  l r = let low = get_low_reg r in
            conn_inst "cmpl" l r ++ ["\tsetg\t"  ++ low] ++ conn_inst "movzbl" low r
         bind Ls  l r = let low = get_low_reg r in
            conn_inst "cmpl" l r ++ ["\tsetl\t"  ++ low] ++ conn_inst "movzbl" low r
         bind GE  l r = let low = get_low_reg r in
            conn_inst "cmpl" l r ++ ["\tsetge\t" ++ low] ++ conn_inst "movzbl" low r
         bind LE  l r = let low = get_low_reg r in
            conn_inst "cmpl" l r ++ ["\tsetle\t" ++ low] ++ conn_inst "movzbl" low r
         bind Equ l r = let low = get_low_reg r in
            conn_inst "cmpl" l r ++ ["\tsete\t"  ++ low] ++ conn_inst "movzbl" low r
         bind Neq l r = let low = get_low_reg r in
            conn_inst "cmpl" l r ++ ["\tsetne\t" ++ low] ++ conn_inst "movzbl" low r
         bind And l r = let low = get_low_reg r in
            conn_inst "andl" l r ++ conn_inst "cmpl" "$0" r ++ ["\tsetne\t" ++ low] ++ conn_inst "movzbl" low r
         bind Or  l r = let low = get_low_reg r in
            conn_inst "orl"  l r ++ conn_inst "cmpl" "$0" r ++ ["\tsetne\t" ++ low] ++ conn_inst "movzbl" low r
         bind Add l r = conn_inst "addl"  l r
         bind Sub l r = conn_inst "subl"  l r
         bind Mul l r = conn_inst "imull" l r
         bind Div l r = let r' = get_free_reg [l, r, "%eax"] in
            if l == "%eax" then
                ["\tcltq", "\tidivl\t" ++ r] ++ conn_inst "movl" l r
            else
                if r == "%eax" then
                    conn_inst "movl" r r' ++ conn_inst "movl" l "%eax" ++ ["\tcltq", "\tidivl\t" ++ r']
                else
                    conn_inst "movl" l "%eax" ++ ["\tcltq", "\tidivl\t" ++ r] ++ conn_inst "movl" "%eax" r

         conn_inst cmd l r = ["\t" ++ cmd ++ "\t" ++ l ++ ", " ++ r]

cExpr (UnaryNode Not e) rgt =
    let
        (expr, reg) = cExpr e rgt
        reg' = get_free_reg [reg]
        low = get_low_reg reg'
    in
        (["\tcmpl\t$0, " ++ reg, "\tsete\t" ++ low, "\tmovabsq\t" ++ low ++ ", " ++ reg'], reg')

cExpr (Number n) rgt = (["\tmovl\t$" ++ show n ++ ", %esi"], "%esi")
-- cExpr (Number n) rgt = ([], "$" ++ show n)

cExpr (Array (Identifier i) e) rgt =
    let
        (expr, reg) = cExpr e rgt
        mov_inst = (if reg == "%eax" then [] else ["\tmovl\t" ++ reg ++ ", %eax"]) ++ ["\tcltq"]
        regf = get_free_reg ["%eax", "%edx", reg]
        move_to_free r = ["\tmovl\t" ++ r ++ ", " ++ regf]
    in
        case Map.lookup i rgt of
            Just (r, s) ->
                if "(%rip)" `isSuffixOf` r
                then
                    (expr ++ mov_inst ++
                     ["\tleaq\t0(,%rax," ++ show s ++ "), %rdx", "\tleaq\t" ++ r ++ ", %rax"] ++
                     move_to_free "(%rdx,%rax)"
                    , regf)
                else
                    (expr ++ mov_inst ++ move_to_free (init r ++ ",%rax," ++ show s ++ ")"), regf)
            _ -> error $ "an error occurred on array-expr"

cExpr (Identifier i) rgt = ([], (\(Just (a, _)) -> a) $ Map.lookup i rgt)

cExpr fc@(FuncCall _ _) rgt = (fst $ cAStmt fc rgt, "%eax")

-- Note: for-stmt, ret-stmt and write-stmt have empty-expr
--       return %eax just for ret-stmt
cExpr Empty _ = ([], "%eax")

