module Grammar where

import Parser
import Data.Char
import Control.Monad
import Control.Exception
import Control.Applicative

data Ast = Number Int
         | BinNode Op Ast Ast
         | UnaryNode Op Ast

data Op = Not | Neg
        | Mul | Div
        | Add | Sub 
        | Gt  | Ls  | GE  | LE  | Equ | Neq
        | And | Or

instance Show Op where
    show Not = "!"
    show Neg = "-"

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
    show (Number x) = show x
    show (BinNode op l r) = "(" ++ show op ++ " " ++ show l  ++ " " ++ show r++ ")"

expr = bool_expr

----------------------------------------------------------
-- Expression
bool_expr :: Parser Ast
bool_expr = chainl (pBinVal string "&&" And <|> pBinVal string "||" Or) cmp_expr

cmp_expr :: Parser Ast
cmp_expr = chainl (pBinVal string ">=" GE <|> pBinVal string "<=" LE <|>
                   pBinVal string ">"  Gt <|> pBinVal string "<"  Ls <|>
                   pBinVal string "==" Equ<|> pBinVal string "!=" Neq)  arith_expr

arith_expr :: Parser Ast
arith_expr = Parser $ \inp ->
    parse (chainl (pBinVal char '+' Add <|> pBinVal char '-' Sub) factor) (fix inp) where
        fix "" = ""
        fix p@(x:xs) = case x of
            '-' -> "0" ++ p
            _   -> p

factor :: Parser Ast
factor = chainl (pBinVal char '*' Mul <|> pBinVal char '/' Div) term

term :: Parser Ast
term = Number <$> integer <|> between (char '(') expr (char ')')

pBinNode = return . BinNode
pBinVal f a b = f a >> pBinNode b
----------------------------------------------------------
    

