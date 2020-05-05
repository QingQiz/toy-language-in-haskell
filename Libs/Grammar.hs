module Grammar(buildAst) where

import Ast
import Parser

import Data.Char
import Control.Monad
import Control.Applicative


buildAst = parse program

-- id
ident  = Identifier <$> id_parser where
    id_parser = (++)
        <$> (many space >> some (letter <?> char '_' `catchPErr` "Excepted char '_' or a letter"))
        <*> (many (letter <?> digit <?> char '_' `catchPErr` "Excepted char '_' or a letter or a digit"))

-- type
pType = (spcStr "int " >> return TInt) <?> (spcStr "char " >> return TChar)

-- char
ch = fmap Ch $ spcChar '\'' *> (satisfy $ \_ -> True) <* char '\''

-- signed int , unsigned int
int  = Number <$> (many space >> sInt)
uint = Number <$> (many space >> uInt)

-- string
str = fmap Str $ (spcChar '"' *> valid <* char '"') where
    valid = many $ satisfy $ \inp ->
        let o = ord inp in o == 32 || o == 33 || (35 <= o && o <= 126)

-- nothing
nothing = string "" >> return Empty

-- array
array = Array <$> ident
    <*> (spcChar '[' *> expr <* spcChar ']') `catchPErr` "Excepted a array (like `id[expr]')"

-- expression, cond-expr
expr = bool_expr
cond = bool_expr

-- bool-expr
bool_expr = chainl (pBinValS "&&" And <?> pBinValS "||" Or) cmp_expr `catchPErr` "Excepted a bool-expr" where
    pBinValS = pBinVal spcStr

-- cmp-expr
cmp_expr = let pBinValS = pBinVal spcStr in
    chainl (pBinValS ">=" GE <?> pBinValS "<=" LE <?>
            pBinValS ">"  Gt <?> pBinValS "<"  Ls <?>
            pBinValS "==" Equ<?> pBinValS "!=" Neq
            `catchPErr` "Excepted a comparison symbol") arith_expr `catchPErr` "Excepted a comparison-expr"

-- arith-expr
arith_expr = let pBinValC = pBinVal spcChar in
    chainl (pBinValC '+' Add <?> pBinValC '-' Sub `catchPErr` "Excepted char '+' or char '-'") factor
    `catchPErr` "Excepted a addition-expr or subtraction-expr"

-- factor-expr
factor = chainl (pBinVal spcChar '*' Mul <?> pBinVal spcChar '/' Div
                `catchPErr` "Excepted char '*' or char '/'") unary_expr
    `catchPErr` "Excepted a multiplication-expr or divison-expr"

-- unary-expr
unary_expr = unaryOpChain (pUnaryValue spcChar '!' Not <?>
                           pUnaryValue spcChar '-' Neg
                           `catchPErr` "Excepted char '!' or char '-'") term

-- term
term = int <?> (spcChar '(' *> expr <* spcChar ')') <?> func_call <?> array <?> ident <?> ch

-- stmt
stmt = if_stmt <?> (spcChar '{' *> stmt_list <* spcChar '}') <?> loop_stmt
    <?> ((rd <?> wt <?> ret <?> func_call <?> assign <?> brk <?> ctn <?> nothing) <* spcChar ';')

-- break & continue
brk = spcStr "break" >> return Break
ctn = spcStr "continue" >> return Continue

-- assign
assign = Assign
    <$> (array <?> ident `catchPErr` "Excepted a array or a Identifier")
    <*> (spcChar '=' >> expr)

-- comd-stmt
comd_stmt = ComdStmt <$> const_desc <*> var_desc <*> stmt_list

-- stmt-list
stmt_list = fmap StmtList $ many stmt

-- if-stmt
if_stmt = IfStmt
    <$> (spcStr "if" >> spcChar '(' *> cond <* spcChar ')')
    <*> (many space >> stmt)
    <*> ((spcStr "else" >> some space >> stmt) <?> nothing)

-- loop-stmt
loop_stmt = do_stmt <?> for_stmt

for_stmt = ForStmt
    <$> (spcStr "for" >> spcChar '(' >> (assign <?> nothing))
    <*> (spcChar ';' >> (cond <?> nothing))
    <*> (spcChar ';' >> (assign <?> nothing))
    <*> (spcChar ')' >> many space >> stmt)

do_stmt = DoStmt
    <$> (spcStr "do" >> stmt)
    <*> (spcStr "while" >> spcChar '(' >> cond <* (spcChar ')' <* spcChar ';'))

-- func-call
func_call = FuncCall <$> ident <*> (spcChar '(' >> sepByE (spcChar ',') expr <* spcChar ')')

-- ret-sttm
ret = Ret <$> (spcStr "return" >> (some space >> expr) <?> nothing)

-- read-stmt
-- not support for array
rd = Rd <$> (spcStr "scanf" >> spcChar '(' *> sepBy (spcChar ',') ident <* spcChar ')')

-- write-stmt
wt = Wt <$> (spcStr "printf" >> spcChar '(' >> str) <*> (many (spcChar ',' *> expr) <* spcChar ')')

-- program
program = Program <$> const_desc <*> var_desc <*> (many func_def <* many space)

-- const reputation
const_desc = many (spcStr "const " >> const_def <* spcChar ';')

const_def  = ((spcStr "int " >> pConstNode TInt) <?> (spcStr "char " >> pConstNode TChar))
    <*> sepBy (spcChar ',') ((,) <$> ident <*> (int <?> ch))

-- variable reputation
var_desc = many (var_def <* spcChar ';')

var_def = (spcStr "int" >> pVarNode TInt) <?> (spcStr "char" >> pVarNode TChar)
    <*> sepBy (spcChar ',') (array_def <?> ident)
    where
        array_def = Array <$> ident <*> (spcChar '[' >> uint <* spcChar ']')

-- func_def :: Parser Ast
func_def = (many space >> (pFuncVal "int " FInt <?> pFuncVal "char " FChar <?> pFuncVal "void " FVoid))
    <*> ident
    <*> (spcChar '(' >> sepByE (spcChar ',') ((,) <$> pType <*> ident) <* spcChar ')')
    <*> (spcChar '{' >> comd_stmt <* spcChar '}')

--  help functions
pUnaryNode = return . UnaryNode
pUnaryValue f a b = f a >> pUnaryNode b

pBinNode = return . BinNode
pBinVal f a b = f a >> pBinNode b

pConstNode = return . ConstDef

pVarNode = return . VarDef

pFuncNode = return . FuncDef
pFuncVal s t = spcStr s >> pFuncNode t

