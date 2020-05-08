module Grammar where

import Ast
import Parser

import Data.Char
import Control.Monad
import Control.Monad.Except
import Control.Applicative


buildAst = parse program

-- id
ident  = Identifier <$> id_parser where
    id_parser = (++)
        <$> (many space >> some (letter <?> char '_' </> "Excepted char '_' or a `letter'"))
        <*> (many (letter <?> digit <?> char '_' </> "Excepted char '_' or a `letter' or a `digit'"))

-- type
pType = (strWithSpc "int" >> return TInt) <?> (strWithSpc "char" >> return TChar)
    </> "Excepted string \"int\" or string \"char\""

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
    <*> (spcChar '[' *> expr <* spcChar ']') </> "Excepted a `array'"

-- expression, cond-expr
expr = bool_expr </> "Excepted a `expression'"
cond = bool_expr </> "Excepted a `condition'"

-- bool-expr
bool_expr = chainl (pBinValS "&&" And <?> pBinValS "||" Or) cmp_expr </> "Excepted a `bool-expr'" where
    pBinValS = pBinVal spcStr

-- cmp-expr
cmp_expr = let pBinValS = pBinVal spcStr in
    chainl (pBinValS ">=" GE <?> pBinValS "<=" LE <?>
            pBinValS ">"  Gt <?> pBinValS "<"  Ls <?>
            pBinValS "==" Equ<?> pBinValS "!=" Neq
            </> "Excepted a `comparison symbol'") arith_expr </> "Excepted a `comparison-expr'"

-- arith-expr
arith_expr = let pBinValC = pBinVal spcChar in
    chainl (pBinValC '+' Add <?> pBinValC '-' Sub </> "Excepted char '+' or char '-'") factor
    </> "Excepted a `addition-expr' or `subtraction-expr'"

-- factor-expr
factor = chainl (pBinVal spcChar '*' Mul <?> pBinVal spcChar '/' Div
                </> "Excepted char '*' or char '/'") unary_expr
    </> "Excepted a `multiplication-expr' or `divison-expr'"

-- unary-expr
unary_expr = unaryOpChain (pUnaryValue spcChar '!' Not <?>
                           pUnaryValue spcChar '-' Neg
                           </> "Excepted char '!' or char '-'") term

-- term
term = int <?> (spcChar '(' *> expr <* spcChar ')') <?> func_call <?> array <?> ident <?> ch
    </> "Excepted a(n) `integer' or `expr' or `function call' or `array' or `identifier' or `char'"

-- stmt
stmt = if_stmt <?> (spcChar '{' *> stmt_list <* spcChar '}') <?> loop_stmt
    <?> ((rd <?> wt <?> ret <?> func_call <?> assign <?> brk <?> ctn <?> nothing) <* spcChar ';')
    </> "Excepted a `statement' or `statement-list'"

-- break & continue
brk = spcStr "break" >> return Break
ctn = spcStr "continue" >> return Continue

-- assign
assign = Assign
    <$> (array <?> ident </> "Excepted an `array' or `identifier'")
    <*> (spcChar '=' >> expr)
    </> "Excepted an `assign-stmt'"

-- comd-stmt
comd_stmt  = ComdStmt <$> const_desc <*> var_desc <*> stmt_list
-- comd-stmt but match with stmt-list'
comd_stmt' = ComdStmt <$> const_desc <*> var_desc <*> stmt_list'

-- stmt-list
stmt_list  = fmap StmtList $ many stmt
-- stmt-list but can't be empty
stmt_list' = fmap StmtList $ some stmt

-- if-stmt
if_stmt = IfStmt
    <$> (spcStr "if" >> spcChar '(' *> cond <* spcChar ')')
    <*> (many space >> stmt)
    <*> ((strWithSpc "else" >> stmt) <?> nothing)
    </> "Excepted a `if-stmt'"

-- loop-stmt
loop_stmt = do_stmt <?> for_stmt </> "Excepted a `loop-stmt'"

for_stmt = ForStmt
    <$> (spcStr "for" >> spcChar '(' >> (assign <?> nothing))
    <*> (spcChar ';' >> (cond <?> nothing))
    <*> (spcChar ';' >> (assign <?> nothing))
    <*> (spcChar ')' >> many space >> stmt)
    </> "Excepted a `for-stmt'"

do_stmt = DoStmt
    <$> (spcStr "do" >> stmt)
    <*> (spcStr "while" >> spcChar '(' >> cond <* (spcChar ')' <* spcChar ';'))
    </> "Excepted a `do-stmt'"

-- func-call
func_call = FuncCall <$> ident <*> (spcChar '(' >> sepByE (spcChar ',') expr <* spcChar ')')
    </> "Excepted a `function-call'"

-- ret-sttm
ret = Ret <$> (spcStr "return" >> (some space >> expr) <?> nothing)
    </> "Excepted a `return-stmt'"

-- read-stmt
-- not support for array
rd = Rd <$> (spcStr "scanf" >> spcChar '(' *> sepBy (spcChar ',') ident <* spcChar ')')
    </> "Excepted a `read-stmt'"

-- write-stmt
wt = Wt <$> (spcStr "printf" >> spcChar '(' >> str) <*> (many (spcChar ',' *> expr) <* spcChar ')')
    </> "Excepted a `write-stmt'"

-- program
program = Program <$> const_desc <*> var_desc <*> (some func_def <* (many space >> eof))

-- const reputation
const_desc = many (strWithSpc "const" >> const_def <* spcChar ';')

const_def  = ((strWithSpc "int" >> pConstNode TInt) <?> (strWithSpc "char" >> pConstNode TChar))
    <*> sepBy (spcChar ',') ((,) <$> ident <*> (spcChar '=' >> int <?> ch))

-- variable reputation
var_desc = many (var_def <* spcChar ';')

var_def = (strWithSpc "int" >> pVarNode TInt) <?> (strWithSpc "char" >> pVarNode TChar)
    <*> sepBy (spcChar ',') (array_def <?> ident)
    where
        array_def = Array <$> ident <*> (spcChar '[' >> uint <* spcChar ']')

-- function-defination
func_def = (many space >> (pFuncVal "int" FInt <?> pFuncVal "char" FChar <?> pFuncVal "void" FVoid))
    <*> ident
    <*> (spcChar '(' >> sepByE (spcChar ',') ((,) <$> pType <*> ident) <* spcChar ')')
    -- If error on }, comd-stmt must match to empty.
    -- then we match with comd_stmt', it will throw the true exception.
    <*> (spcChar '{' >> comd_stmt <* (spcChar '}' `catchError` \_ -> comd_stmt' >> spcChar '}'))
    </> "Excepted a `function-defination'"

--  help functions
pUnaryNode = return . UnaryNode
pUnaryValue f a b = f a >> pUnaryNode b

pBinNode = return . BinNode
pBinVal f a b = f a >> pBinNode b

pConstNode = return . ConstDef

pVarNode = return . VarDef

pFuncNode = return . FuncDef
pFuncVal s t = strWithSpc s >> pFuncNode t

