module Grammar (build_ast) where

import Parser
import ParserAst

import Data.Char
import Control.Monad
import Control.Exception
import Control.Applicative


----------------------------------------------------------
-- basic parser
char :: Char -> Parser Char
char c = satisfy (==c)
spcChar inp = many space >> char inp

space = satisfy isSpace
digit = satisfy isDigit
letter = satisfy isLetter

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (`elem` cs)

string :: String -> Parser String
string "" = return ""
string str@(x:xs) = do
    s <- char x
    ss <- string xs
    return str
spcStr  inp = many space >> string inp


-- parse sth divied by sth
sepBy :: Parser sep -> Parser a -> Parser [a]
sepBy sep a = do
    m1 <- a
    m2 <- many (sep >> a)
    return $ m1 : m2


-- parse sth divied by sth (result can be [])
sepByE :: Parser sep -> Parser a -> Parser [a]
sepByE sep a = sepBy sep a <|> return []


-- parse the left combined chain
chainl :: Parser (a -> a -> a) -> Parser a -> Parser a
chainl op p = do
    x <- many space >> p
    for_rest x where
        for_rest x = (do
            f <- many space >> op
            y <- p
            for_rest $ f x y) <|> return x


-- parse unary operation chain
unaryOpChain:: Parser (a -> a) -> Parser a -> Parser a
unaryOpChain op p = do
    f <- many space >> many (many space >> op)
    x <- p
    return $ func f x where
        func [] x = x
        func (f1:fs) x = func fs $ f1 x


number_n0 :: Parser Char
number_n0 = oneOf "123456789"


u_integer :: Parser Int
u_integer = read <$> ((do
    d1 <- many space >> number_n0
    dx <- many digit
    return $ d1 : dx) <|> string "0")


integer :: Parser Int
integer = do
    many space
    op <- string "+" <|> string "-" <|> string ""
    d  <- many space >> u_integer
    case op of
        ""  -> return d
        "+" -> return d
        "-" -> return (-1 * d)


----------------------------------------------------------
-- sig
ident :: Parser Ast
ident  = Identifier <$> id_parser where
    id_parser = do
        f1 <- many space >> some (letter <|> char '_')
        fo <- many (letter <|> digit <|> char '_')
        return $ f1 ++ fo


ch :: Parser Ast
ch = fmap Ch $ (spcChar '\'' *> valid <* char '\'') where
    valid = satisfy $ \inp -> True


str :: Parser Ast
str = fmap Str $ (spcChar '"' *> valid <* char '"') where
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
    r <- spcChar '[' *> expr <* spcChar ']'
    return $ Array l r


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
term = Number <$> integer  <|> (spcChar '(' *> expr <* spcChar ')') <|>
       func_call <|> array <|> ident <|> ch


----------------------------------------------------------
-- stmt
stmt :: Parser Ast
stmt = if_stmt <|> (spcChar '{' *> stmt_list <* spcChar '}') <|> loop_stmt  <|>
      ((rd <|> wt) <* spcChar ';') <|> -- read or write
      (ret         <* spcChar ';') <|> -- ret stmt
      (func_call   <* spcChar ';') <|> -- func_call
      (nothing     <* spcChar ';') <|> -- empty
      (assign      <* spcChar ';')     -- assig_stmt


assign :: Parser Ast
assign = do
    l <- array <|> ident
    spcChar '='
    r <- expr
    return $ Assign l r


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
    c  <- spcChar '(' *> cond <* spcChar ')'
    cs <- many space >> stmt
    es <- many space >> ((string "else " >> stmt) <|> nothing)
    return $ IfStmt c cs es


loop_stmt :: Parser Ast
loop_stmt = for_stmt <|> do_stmt where
    for_stmt = do
        spcStr "for" >> spcChar '('
        a1 <- assign <|> nothing
        spcChar ';'
        c  <- cond   <|> nothing
        spcChar ';'
        a2 <- assign <|> nothing
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
    r <- spcChar '(' *> valid <* spcChar ')'
    return $ FuncCall l r ) where
    valid = sepByE (spcChar ',') expr


ret :: Parser Ast
ret = do
    spcStr "return"
    e <- (spcChar '(' *> expr <* spcChar ')') <|> nothing
    return $ Ret e


rd :: Parser Ast
rd = do
    spcStr "scanf"
    ids <- spcChar '(' *> sepBy (spcChar ',') ident <* spcChar ')'
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
    many space
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
        r <- spcChar '[' *> uint <* spcChar ']'
        return $ Array l r


func_def :: Parser Ast
func_def = (do
    many space
    f <- pFuncVal "int " FInt <|> pFuncVal "char " FChar <|> pFuncVal "void " FVoid
    fn <- ident
    pl <- spcChar '(' *> valid <* spcChar ')'
    fb <- spcChar '{' *> comd_stmt <* spcChar '}'
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

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ func where
    func "" = Nothing
    func (x:xs) | f x = Just (x, xs)
                | otherwise = Nothing

build_ast :: String -> Maybe (Ast, String)
build_ast = parse program


