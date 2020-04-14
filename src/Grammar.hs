module Grammar where

import Parser
import Data.Char
import Control.Monad
import Control.Exception
import Control.Applicative

data Ast = Number Int
         | ExpNode Op Ast Ast

data Op = Add | Sub | Mul | Div
        | Gt  | Ls  | GE  | LE  | Equ | Neq
        | And | Or  

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Gt = ">"
    show Ls = "<"
    show GE = ">="
    show LE = "<="
    show Equ = "=="
    show Neq = "!="
    show And = "&&"
    show Or = "||"

instance Show Ast where
    show (Number x) = show x
    show (ExpNode op l r) = "(" ++ show op ++ " " ++ show l  ++ " " ++ show r++ ")"

expr = bool_expr

----------------------------------------------------------
-- Expression
bool_expr :: Parser Ast
bool_expr = chainl (pExpVal string "&&" And <|> pExpVal string "||" Or) cmp_expr

cmp_expr :: Parser Ast
cmp_expr = chainl (pExpVal string ">=" GE <|> pExpVal string "<=" LE <|>
                   pExpVal string ">"  Gt <|> pExpVal string "<"  Ls <|>
                   pExpVal string "==" Equ<|> pExpVal string "!=" Neq)  arith_expr

arith_expr :: Parser Ast
arith_expr = chainl (pExpVal char '+' Add <|> pExpVal char '-' Sub) factor

factor :: Parser Ast
factor = chainl (pExpVal char '*' Mul <|> pExpVal char '/' Div) term

term :: Parser Ast
term = Number <$> u_integer <|> between (char '(') expr (char ')')

pExpNode = return . ExpNode
pExpVal f a b = f a >> pExpNode b
----------------------------------------------------------
    

