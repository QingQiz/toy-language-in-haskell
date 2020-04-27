module Ast where

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
         | ConstDef   Type [(Ast, Ast)] -- ConstDef type [(id, int | ch)]
         | VarDef     Type [Ast] -- VarDef type [dec]
        -- FuncDef  ret_type name [(param_type, param_name)] func_body
         | FuncDef    FunType Ast [(Type, Ast)] Ast
         | FuncCall   Ast [Ast] -- FuncCall ident params
         | Ret        Ast       -- return expr
         | Rd         [Ast]     -- read [id]
         | Wt         Ast [Ast] -- write fmt-str [expr]
         deriving (Show)


data Op = Not
        | Mul | Div
        | Add | Sub
        | Gt  | Ls  | GE  | LE  | Equ | Neq
        | And | Or
        deriving (Show)


data Type = TInt | TChar deriving (Show)


data FunType = FInt | FChar | FVoid deriving (Show)

