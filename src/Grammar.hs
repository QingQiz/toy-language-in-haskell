module Grammar where

import Parser
import Data.Char
import Control.Monad
import Control.Exception
import Control.Applicative


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


----------------------------------------------------------
-- sig
ident :: Parser Ast
ident  = Identifier <$> id_parser where
    id_parser = do
        f1 <- many space >> some (letter <|> char '_')
        fo <- many (letter <|> digit <|> char '_')
        return $ f1 ++ fo


ch :: Parser Ast
ch = fmap Ch $ between (spcChar '\'') valid (char '\'') where
    valid = satisfy $ \inp -> True


str :: Parser Ast
str = fmap Str $ between (spcChar '"') valid (char '"') where
    valid = many $ satisfy (\inp ->
        let o = ord inp in
            if o == 32 || o == 33 || (35 <= o && o <= 126)
            then True
            else False)


int :: Parser Ast
int = Number <$> integer


uint :: Parser Ast
uint = Number <$> u_integer


array :: Parser Ast
array = do
    l <- ident
    r <- between (spcChar '[') expr (spcChar ']')
    return $ Array l r


assign :: Parser Ast
assign = do
    l <- array <|> ident
    spcChar '='
    r <- expr
    return $ Assign l r


nothing :: Parser Ast
nothing = string "" >> return Empty


----------------------------------------------------------
-- expression
expr = bool_expr
cond = bool_expr


bool_expr :: Parser Ast
bool_expr = chainl (pBinValS "&&" And <|> pBinValS "||" Or) cmp_expr where
    pBinValS = pBinVal string


cmp_expr :: Parser Ast
cmp_expr = let pBinValS = pBinVal string in
    chainl (pBinValS ">=" GE <|> pBinValS "<=" LE <|>
            pBinValS ">"  Gt <|> pBinValS "<"  Ls <|>
            pBinValS "==" Equ<|> pBinValS "!=" Neq) arith_expr


arith_expr :: Parser Ast
arith_expr = Parser $ \inp ->
    parse (chainl (pBinValC '+' Add <|> pBinValC '-' Sub) factor) (fix inp) where
        fix "" = ""
        fix p@(x:xs) = case x of
            '-' -> "0" ++ p
            _   -> p
        pBinValC = pBinVal char


factor :: Parser Ast
factor = chainl (pBinVal char '*' Mul <|> pBinVal char '/' Div) unary_expr


unary_expr :: Parser Ast
unary_expr = unaryOpChain (pUnaryValue char '!' Not) term


term :: Parser Ast
term = (Number <$> integer <|> between (spcChar '(') expr (spcChar ')')) <|>
       func_call <|> array <|> ident <|> ch


----------------------------------------------------------
-- stmt
stmt :: Parser Ast
stmt = if_stmt <|> between (spcChar '{') stmt_list (spcChar '}') <|> loop_stmt  <|>
      ((rd <|> wt) <* spcChar ';') <|> -- read or write
      (ret         <* spcChar ';') <|> -- ret stmt
      (func_call   <* spcChar ';') <|> -- func_call
      (nothing     <* spcChar ';') <|> -- empty
      (assign      <* spcChar ';')     -- assig_stmt


comd_stmt :: Parser Ast
comd_stmt = do
    cd <- const_desc
    vd <- var_desc
    sl <- stmt_list
    return $ ComdStmt cd vd sl


stmt_list :: Parser Ast
stmt_list = fmap StmtList $ many stmt


if_stmt :: Parser Ast
if_stmt = do
    spcStr "if"
    c  <- between (spcChar '(') cond (spcChar ')')
    cs <- many space >> stmt
    es <- many space >> ((string "else " >> stmt) <|> nothing)
    return $ IfStmt c cs es


loop_stmt :: Parser Ast
loop_stmt = for_stmt <|> do_stmt where
    for_stmt = do
        spcStr "for" >> spcChar '('
        a1 <- assign
        spcChar ';'
        c  <- cond
        spcChar ';'
        a2 <- assign
        spcChar  ')'
        s  <- stmt
        return $ ForStmt a1 c a2 s
    do_stmt  = do
        spcStr "do "
        s <- stmt
        spcStr "while" >> spcChar '('
        e <- cond
        spcChar ')' >> spcChar ';'
        return $ DoStmt s e


func_call :: Parser Ast
func_call = (do
    l <- ident
    r <- between (spcChar '(') valid (spcChar ')')
    return $ FuncCall l r ) where
    valid = sepByE (spcChar ',') expr


ret :: Parser Ast
ret = do
    spcStr "return "
    e <- between (spcChar '(') expr (spcChar ')')
    return $ Ret e


rd :: Parser Ast
rd = do
    spcStr "scanf"
    ids <- between (spcChar '(') (sepBy (spcChar ',') ident) (spcChar ')')
    return $ Rd ids


wt :: Parser Ast
wt = do
    spcStr "printf"
    spcChar '('
    (do s <- str
        spcChar ','
        e <- expr
        spcChar ')'
        return $ Wt s e) <|> (do
        e <- expr
        spcChar ')'
        return $ Wt Empty e) <|> (do
        s <- str
        spcChar '('
        return $ Wt s Empty)


----------------------------------------------------------
-- program
program :: Parser Ast
program = do
    cd <- const_desc
    vd <- var_desc
    fd <- many func_def
    return $ Program cd vd fd


const_desc :: Parser [Ast]
const_desc = many (spcStr "const " >> const_def <* spcChar ';')


const_def  :: Parser Ast
const_def  = (do
    many space
    f <- (string "int " >> pConstNode TInt) <|> (string "char " >> pConstNode TChar)
    r <- sepBy (spcChar ',') pone
    return $ f r) where
    pone = do
        l <- ident
        spcChar '='
        r <- int <|> ch
        return $ (l, r)


var_desc :: Parser [Ast]
var_desc = many (var_def <* spcChar ';')


var_def :: Parser Ast
var_def = (do
    many space
    f <- (string "int " >> pVarNode TInt) <|> (string "char " >> pVarNode TChar)
    r <- sepBy (spcChar ',') (array_def <|> ident)
    return $ f r) where
    array_def = do
        l <- ident
        r <- between (spcChar '[') uint (spcChar ']')
        return $ Array l r


func_def :: Parser Ast
func_def = (do
    many space
    f <- pFuncVal "int " FInt <|> pFuncVal "char " FChar <|> pFuncVal "void " FVoid
    fn <- ident
    pl <- between (spcChar '(') valid (spcChar ')')
    fb <- between (spcChar '{') comd_stmt (spcChar '}')
    return $ f fn pl fb) where
    valid = sepByE (spcChar ',') def_header
    def_header = do
        t <- (spcStr "int " >> return TInt) <|> (spcStr "char " >> return TChar)
        id <- ident
        return $ (t, id)


----------------------------------------------------------
--  help functions
pUnaryNode = return . UnaryNode
pUnaryValue f a b = f a >> pUnaryNode b

pBinNode = return . BinNode
pBinVal f a b = f a >> pBinNode b

pConstNode = return . ConstDef

pVarNode = return . VarDef

pFuncNode = return . FuncDef
pFuncVal s t = spcStr s >> pFuncNode t

spcChar inp = many space >> char inp
spcStr  inp = many space >> string inp


