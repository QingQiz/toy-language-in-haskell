module Test where

import qualified Data.Map as Map
import PeepHole
import RegisterAlloca
import Ast
import Parser
import Grammar
import Simplify
import Semantic
import Register
import CodeGen
import Symbol
import CFG
import Functions
import Optimizer
import Data.List
import TAC
import Livness

cmd_f = put_cg $ optimize code_f
cmd   = put_cg $ optimize code
cmd'  = put_cg $ optimize code'
cmd'' = put_cg $ optimize code''

for_all = put_cg . finalDash . optimize . full_cg

for_code = put_cg . optimize . full_cg
p_code = put_cg . full_cg

cls = putStr "\ESC[2J"

code_f = full_cg "int f(int a, int b, int c, int d) {return a + b + c + d;} int f2() {return f(1,2,3,4);}"
code   = full_cg "int f() {int a,b,c; a=1; if(b) c=2; b=a + 2 - c; return b;}"
code'  = full_cg "int f() {int a,b,c,d; c = a + b + 5; d = a + b + 5; return d;}"
-- code'' = full_cg "int f() {int a, b; a = 5; for (;a<10;a=a+1) b=a; b=a;}"
code'' = full_cg "int f() {int a, b; a = 5; if(b) b=1; b=a; return b;}"

full_cg = cProgram . getM . semaProgram . getR . (parse program)

put_cg = putStrLn . unlines

simp = putExpr . simplify . pExp

pfuncd = getR . (parse func_def)
pExp = getR . (parse expr)

getM = \(Just x) -> x
getR = \(Right x) -> x

putExpr (BinNode op _ a b) = "("++ putExpr a ++ putOp op ++ putExpr b ++")"
putExpr (UnaryNode op _ a) = putOp op ++ "("++putExpr a++")"
putExpr (Number _ n) = show n
putExpr (Identifier _ n) = n

putOp Add = "+"
putOp Sub = "-"
putOp Mul = "*"
putOp Div = "/"
putOp Neg = "-"
putOp Not = "!"
putOp Gt  = ">"
putOp Ls  = "<"
putOp GE  = ">="
putOp LE  = "<="
putOp Equ = "=="
putOp Neq = "!="
putOp And = "&&"
putOp Or  = "||"

show' ((a, b):ts) = "\n" ++ (if length a >= 8 then a ++ "" else a ++ "\t") ++ "\t" ++ (if null b then "" else "=\t" ++ b) ++ show' ts
show' [] = "\n"
