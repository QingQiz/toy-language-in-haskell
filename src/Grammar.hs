module Grammar where

import Parser
import Data.Char
import Control.Monad
import Control.Exception
import Control.Applicative

data Ast = Empty
         | Number Int
         | Str String
         | Identifier String
         | BinNode Op Ast Ast
         | UnaryNode Op Ast
         | IfStmt Ast Ast Ast -- ifstmt expr stmt else_stmt
         | StmtList [Ast]
         | ForStmt Ast Ast Ast Ast -- forstmt beg cond step loop_stmt
         | DoStmt Ast Ast -- dostmt loop_stmt cond
         | Array Ast Ast -- array id index
         | Assign Ast Ast -- assign left right

data Op = Not
        | Mul | Div
        | Add | Sub
        | Gt  | Ls  | GE  | LE  | Equ | Neq
        | And | Or

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
    show (Str p) = show p
    show (Identifier p) = show p
    show (BinNode op l r) = "(" ++ show op ++ " " ++ show l ++ " " ++ show r++ ")"
    show (UnaryNode op p) = "(" ++ show op ++ " " ++ show p ++ ")"
    show (IfStmt c s es) = "(if " ++ show c ++ " ; " ++ show s ++ " ; " ++ show es ++ ")"
    show (StmtList ss) = show ss
    show (ForStmt b c s l) = "(for " ++ show b ++ " ; " ++ show c ++ " ; " ++ show s ++ " ; " ++show l ++ ")"
    show (DoStmt l c) = "(do " ++ show l ++ " ; " ++ show c ++ ")"
    show (Array a b) = show a ++ "["++ show b ++"]"
    show (Assign a b) = show a ++ "=" ++ show b


----------------------------------------------------------
-- sig
ident :: Parser Ast
ident  = Identifier <$> id_parser where
    id_parser = do
        f1 <- many space >> some (letter <|> char '_')
        fo <- many (letter <|> digit <|> char '_')
        return $ f1 ++ fo

str :: Parser Ast
str = fmap Str $ skipMany space >> between (char '"') valid (char '"') where
    valid = many $ satisfy (\inp ->
        let o = ord inp in
            if o == 32 || o == 33 || (35 <= o && o <= 126)
            then True
            else False)

array :: Parser Ast
array = do
    l <- ident
    r <- between (many space >> char '[') expr (many space >> char ']')
    return $ Array l r

assign :: Parser Ast
assign = do
    l <- array <|> ident
    many space >> char '='
    r <- expr
    return $ Assign l r


----------------------------------------------------------
-- expression
expr = bool_expr

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
term = (Number <$> integer <|> between (char '(') expr (char ')')) <|>
       array <|> ident


----------------------------------------------------------
-- stmt
stmt :: Parser Ast
stmt = if_stmt <|> stmt_list <|> loop_stmt  <|>
      (nothing <* (many space >> char ';')) <|> -- empty
      (assign  <* (many space >> char ';')) -- assig_stmt

nothing :: Parser Ast
nothing = string "" >> return Empty

stmt_list :: Parser Ast
stmt_list = fmap StmtList $ between open valid close where
    open  = many space >> char '{'
    close = many space >> char '}'
    valid = many (many space >> stmt)
    
if_stmt :: Parser Ast
if_stmt = do
    many space >> string "if"
    cond <- many space >> between (char '(') expr (many space >> char ')')
    cond_stmt <- many space >> stmt
    else_stmt <- many space >> ((string "else" >> stmt) <|> nothing)
    return $ IfStmt cond cond_stmt else_stmt

loop_stmt :: Parser Ast
loop_stmt = for_stmt <|> do_stmt where
    for_stmt = do
        many space >> string "for" >> many space >> char '('
        a1 <- assign
        many space >> char ';'
        c  <- expr
        many space >> char ';'
        a2 <- assign
        many space >> char ')'
        s  <- stmt
        return $ ForStmt a1 c a2 s
    do_stmt  = do
        many space >> string "do"
        s <- stmt
        many space >> string "while" >> many space >> char '('
        e <- expr
        many space >> char ')'
        return $ DoStmt s e


----------------------------------------------------------
--  help functions
pUnaryNode = return . UnaryNode
pUnaryValue f a b = f a >> pUnaryNode b
pBinNode = return . BinNode
pBinVal f a b = f a >> pBinNode b



