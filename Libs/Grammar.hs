module Grammar where

import Ast
import Parser

import Data.Char
import Control.Monad
import Control.Monad.Except
import Control.Applicative


buildAst = parse program

-- id
ident  = Identifier <!> id_parser where
    id_parser = (++)
        <$> (many space >> some (letter <?> char '_'))
        <*> (many (letter <?> digit <?> char '_'))

-- type
pType = (strWithSpc "int" >> return TInt) <?> (strWithSpc "char" >> return TChar)

-- char
ch = Ch <!> (spcChar '\'' *> (satisfy $ \_ -> True) <* char '\'')

-- signed int , unsigned int
int  = Number <!> (many space >> sInt)
uint = Number <!> (many space >> uInt)

-- string
str = Str <!> (spcChar '"' *> valid <* char '"') where
    valid = many $ satisfy $ \inp ->
        let o = ord inp in o == 32 || o == 33 || (35 <= o && o <= 126)

-- nothing
nothing = string "" >> return Empty

-- array
array = Array <!> ident
    <*> (spcChar '[' *> expr <* spcChar ']')

-- expression, cond-expr
expr = bool_expr
cond = bool_expr

-- bool-expr
bool_expr = chainl (pBinValS "&&" And <?> pBinValS "||" Or) cmp_expr where
    pBinValS = pBinVal spcStr

-- cmp-expr
cmp_expr = let pBinValS = pBinVal spcStr in
    chainl (pBinValS ">=" GE <?> pBinValS "<=" LE <?>
            pBinValS ">"  Gt <?> pBinValS "<"  Ls <?>
            pBinValS "==" Equ<?> pBinValS "!=" Neq) arith_expr

-- arith-expr
arith_expr = let pBinValC = pBinVal spcChar in
    chainl (pBinValC '+' Add <?> pBinValC '-' Sub) factor

-- factor-expr
factor = chainl (pBinVal spcChar '*' Mul <?> pBinVal spcChar '/' Div) unary_expr

-- unary-expr
unary_expr = unaryOpChain (pUnaryValue spcChar '!' Not <?> pUnaryValue spcChar '-' Neg) term where
    pUnaryValue f a b = f a >> return (UnaryNode b) <*> peek

-- term
term = int <?> (spcChar '(' *> expr <* spcChar ')') <?> func_call <?> array <?> ident <?> ch

-- stmt
stmt = if_stmt
    <?> (spcChar '{' *> stmt_list <* spcChar '}')
    <?> loop_stmt
    <?> ((rd <?> wt <?> ret <?> func_call <?> assign <?> brk <?> ctn <?> nothing) <* spcChar ';')

-- break & continue
brk = spcStr "break" >> (Break <$> peek)
ctn = spcStr "continue" >> (Continue <$> peek)

-- assign
assign = Assign <!> (array <?> ident) <*> (spcChar '=' >> expr)

-- comd-stmt
comd_stmt  = ComdStmt <!> const_desc <*> var_desc <*> stmt_list

-- stmt-list
stmt_list  = StmtList <!> many stmt

-- if-stmt
if_stmt = IfStmt
    <!> (spcStr "if" >> spcChar '(' *> cond <* spcChar ')')
    <*> (many space >> stmt)
    <*> ((strWithSpc "else" >> stmt) <?> nothing)

-- loop-stmt
loop_stmt = do_stmt <?> for_stmt

for_stmt = ForStmt
    <!> (spcStr "for" >> spcChar '(' >> (assign <?> nothing))
    <*> (spcChar ';' >> (cond <?> nothing))
    <*> (spcChar ';' >> (assign <?> nothing))
    <*> (spcChar ')' >> many space >> stmt)

do_stmt = DoStmt
    <!> (spcStr "do" >> stmt)
    <*> (spcStr "while" >> spcChar '(' >> cond <* (spcChar ')' <* spcChar ';'))

-- func-call
func_call = FuncCall <!> ident <*> (spcChar '(' >> sepByE (spcChar ',') expr <* spcChar ')')

-- ret-sttm
ret = Ret <!> (spcStr "return" >> (some space >> expr) <?> nothing)

-- read-stmt
-- not support for array
rd = Rd <!> (spcStr "scanf" >> spcChar '(' *> sepBy (spcChar ',') ident <* spcChar ')')

-- write-stmt
wt = Wt <!> (spcStr "printf" >> spcChar '(' >> str) <*> (many (spcChar ',' *> expr) <* spcChar ')')

-- program
program = Program <$> peekSC <*> const_desc <*> var_desc <*> (some func_def <* (many space >> eof))

-- const reputation
const_desc = many (strWithSpc "const" >> const_def <* spcChar ';')

const_def  = let pConstVal = pVal ConstDef in
    ((pConstVal "int" TInt) <?> (pConstVal "char" TChar))
    <*> sepBy (spcChar ',') ((,) <$> ident <*> (spcChar '=' >> int <?> ch))

-- variable reputation
var_desc = many (var_def <* spcChar ';')

var_def = (pVarVal "int" TInt) <?> (pVarVal "char" TChar)
    <*> sepBy (spcChar ',') (array_def <?> ident)
    where
        array_def = Array <!> ident <*> (spcChar '[' >> uint <* spcChar ']')
        pVarVal = pVal VarDef

-- function-defination
func_def = let pFuncVal = pVal FuncDef in
    (many space >> (pFuncVal "int" FInt <?> pFuncVal "char" FChar <?> pFuncVal "void" FVoid))
    <*> ident
    <*> (spcChar '(' >> sepByE (spcChar ',') ((,) <$> pType <*> ident) <* spcChar ')')
    <*> (spcChar '{' >> comd_stmt <* spcChar '}')

--  help functions
pBinVal f a b = peek >>= (\pk -> f a >> return pk) >>= (\pk -> return (BinNode b pk))

pVal vc s t = peek >>= (\f -> strWithSpc s >> return f) >>= (\f -> return $ vc t f)

infixl 4 <!>
a <!> b = a <$> peek <*> b

