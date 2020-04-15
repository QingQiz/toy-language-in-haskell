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
    show (IfStmt c s es) = "(if (" ++ show c ++ ") (" ++ show s ++ ") (" ++ show es ++ "))"


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
term = (Number <$> integer <|> between (char '(') expr (char ')')) <|> ident

----------------------------------------------------------
-- stmt
stmt :: Parser Ast
stmt = if_stmt <|> (nothing <* (many space >> char ';'))

nothing :: Parser Ast
nothing =
    string "" >> return Empty

stmt_list :: Parser Ast
stmt_list = undefined

if_stmt :: Parser Ast
if_stmt = do
    many space >> string "if"
    cond <- many space >> between (char '(') expr (char ')')
    cond_stmt <- many space >> stmt
    else_stmt <- many space >> ((string "else" >> stmt) <|> nothing)
    return $ IfStmt cond cond_stmt else_stmt




----------------------------------------------------------
--  help functions
pUnaryNode = return . UnaryNode
pUnaryValue f a b = f a >> pUnaryNode b
pBinNode = return . BinNode
pBinVal f a b = f a >> pBinNode b



