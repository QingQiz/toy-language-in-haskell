module Ast where

type Pos = (Int, Int)

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

         | Program    String [Ast] [Ast] [Ast] -- program const_desc var_desc [func_def]
         | ConstDef   Type Pos [(Ast, Ast)] -- ConstDef type [(id, int | ch)]
         | VarDef     Type Pos [Ast] -- VarDef type [dec]
        -- FuncDef  ret_type name [(param_type, param_name)] func_body
         | FuncDef    FunType Pos Ast [(Type, Ast)] Ast
         | FuncCall   Pos Ast [Ast] -- FuncCall ident params
         | Ret        Pos Ast       -- return expr
         | Rd         Pos [Ast]     -- read [id]
         | Wt         Pos Ast [Ast] -- write fmt-str [expr]
         deriving (Show, Eq)


data Op = Not | Neg
        | Mul | Div
        | Add | Sub
        | Gt  | Ls  | GE  | LE  | Equ | Neq
        | And | Or
        deriving (Show, Eq)


data Type = TInt | TChar deriving (Show, Eq)


data FunType = FInt | FChar | FVoid deriving (Show, Eq)

pNum      = Number (0,0)
pCh       = Ch (0,0)
pStr      = Str (0,0)
pId       = Identifier (0,0)
pArr      = Array (0,0)
pIf       = IfStmt (0,0)
pStmtList = StmtList (0,0)
pFor      = ForStmt (0,0)
pDo       = DoStmt (0,0)
pAssign   = Assign (0,0)
pBreak    = Break (0,0)
pContinue = Continue (0,0)
pComd     = ComdStmt (0,0)
pProgram  = Program
pConst    = \t x -> ConstDef t (0,0) x
pVar      = \t x -> VarDef t (0, 0) x
pFunc     = \t a b c -> FuncDef t (0,0) a b c
pFC       = FuncCall (0,0)
pRet      = Ret (0,0)
pRd       = Rd (0,0)
pWt       = Wt (0,0)

pBin      = \op a b -> BinNode op (0,0) a b
pAdd      = pBin Add
pSub      = pBin Sub
pMul      = pBin Mul

pUnary    = \op a -> UnaryNode op (0,0) a
pNot      = pUnary Not
pNeg      = pUnary Neg

