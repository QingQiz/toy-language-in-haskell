module ParserAst where

data Ast = Empty
         | Number     Int
         | Ch         Char
         | Str        String
         | Identifier String
         | Array      Ast Ast -- array id index

         | BinNode    Op Ast Ast
         | UnaryNode  Op Ast

         | IfStmt     Ast Ast Ast -- ifstmt expr stmt else_stmt
         | StmtList   [Ast] -- stmtlist [stmt]
         | ForStmt    Ast Ast Ast Ast -- forstmt beg cond step loop_stmt
         | DoStmt     Ast Ast -- dostmt loop_stmt cond
         | Assign     Ast Ast -- assign left right
         | ComdStmt   [Ast] [Ast] Ast -- comdstmt const_desc var_desc stmt_list

         | Program    [Ast] [Ast] [Ast] -- program const_desc var_desc [func_def]
         | ConstDef   Type [(Ast, Ast)] -- ConstDef type [(l1, r1), (l2, r2)]
         | VarDef     Type [Ast] -- VarDef type [dec]
        -- FuncDef  name ret_type [(param_type, param_name)] func_body
         | FuncDef    FunType Ast [(Type, Ast)] Ast
         | FuncCall   Ast [Ast] -- FuncCall ident params
         | Ret        Ast       -- return expr
         | Rd         [Ast]     -- read [id]
         | Wt         Ast Ast   -- write str expr


data Op = Not
        | Mul | Div
        | Add | Sub
        | Gt  | Ls  | GE  | LE  | Equ | Neq
        | And | Or


data Type = TInt | TChar deriving (Show)


data FunType = FInt | FChar | FVoid deriving (Show)


instance Show Op where
    show Not = "!"

    show Mul = "*"
    show Div = "/"

    show Add = "+"
    show Sub = "-"

    show GE = ">="
    show LE = "<="
    show Gt = ">"
    show Ls = "<"
    show Equ = "=="
    show Neq = "!="

    show And = "&&"
    show Or = "||"

instance Show Ast where
    show Empty = "Empty"
    show (Number x) = show x
    show (Ch x) = show x
    show (Str p) = show p
    show (Identifier p) = "(id " ++ show p ++ ")"
    show (BinNode op l r) = "(" ++ show op ++ " " ++ show l ++ " " ++ show r++ ")"
    show (UnaryNode op p) = "(" ++ show op ++ " " ++ show p ++ ")"
    show (IfStmt c s es) = "(if " ++ show c ++ " ; " ++ show s ++ " ; " ++ show es ++ ")"
    show (StmtList ss) = show ss
    show (ForStmt b c s l) = "(for " ++ show b ++ " ; " ++ show c ++ " ; " ++ show s ++ " ; " ++show l ++ ")"
    show (DoStmt l c) = "(do " ++ show l ++ " ; " ++ show c ++ ")"
    show (Array a b) = "(arr " ++ show a ++ " "++ show b ++ ")"
    show (Assign a b) = "(= " ++ show a ++ " " ++ show b ++ ")"
    show (ConstDef t l) = "(CDef " ++ show t ++ " " ++ show l ++ ")"
    show (VarDef t l) = "(VDef " ++ show t ++ " " ++ show l ++ ")"
    show (FuncDef t n p b) = "(FunDef " ++ show t ++ " " ++ show n ++ " " ++ show p ++ " " ++ show b ++ ")"
    show (ComdStmt c v s) = "(comdstmt " ++ show c ++ " " ++ show v ++ " " ++ show s ++ ")"
    show (Program c v f) = "(Program " ++ show c ++ " " ++ show v ++ " " ++ show f ++ ")"
    show (FuncCall f p) = "(Call " ++ show f ++ " " ++ show p ++ ")"
    show (Ret e) = "(Ret " ++ show e ++ ")"
    show (Rd i) = "(Read  " ++ show i ++ ")"
    show (Wt a b) = "(Write " ++ show a ++ " " ++ show b ++ ")"

