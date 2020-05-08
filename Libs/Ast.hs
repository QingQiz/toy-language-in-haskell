module Ast where

import Parser(Pos)

data Ast = Empty
         | Number     Pos Int
         | Ch         Pos Char
         | Str        Pos String
         | Identifier Pos String
         | Array      Pos Ast Ast -- array id index

         | BinNode    Op Pos Ast Ast
         | UnaryNode  Op Pos Ast

         | IfStmt     Pos Ast Ast Ast -- ifstmt expr stmt else_stmt
         | StmtList   Pos [Ast] -- stmtlist [stmt]
         | ForStmt    Pos Ast Ast Ast Ast -- forstmt beg cond step loop_stmt
         | DoStmt     Pos Ast Ast -- dostmt loop_stmt cond
         | Assign     Pos Ast Ast -- assign left right
         | Break      Pos
         | Continue   Pos
         | ComdStmt   Pos [Ast] [Ast] Ast -- comdstmt const_desc var_desc stmt_list

         | Program    Pos [Ast] [Ast] [Ast] -- program const_desc var_desc [func_def]
         | ConstDef   Type Pos [(Ast, Ast)] -- ConstDef type [(id, int | ch)]
         | VarDef     Type Pos [Ast] -- VarDef type [dec]
        -- FuncDef  ret_type name [(param_type, param_name)] func_body
         | FuncDef    FunType Pos Ast [(Type, Ast)] Ast
         | FuncCall   Pos Ast [Ast] -- FuncCall ident params
         | Ret        Pos Ast       -- return expr
         | Rd         Pos [Ast]     -- read [id]
         | Wt         Pos Ast [Ast] -- write fmt-str [expr]
         deriving (Show)


data Op = Not | Neg
        | Mul | Div
        | Add | Sub
        | Gt  | Ls  | GE  | LE  | Equ | Neq
        | And | Or
        deriving (Show)


data Type = TInt | TChar deriving (Show)


data FunType = FInt | FChar | FVoid deriving (Show)

