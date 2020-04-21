module Symbol where

import Data.Map


-- TODO insert reserve name into empty symbol table
empty_st = fromList ([]::[(String, Symbol)])

----                   name   type
type SymbolTable = Map String Symbol


data Symbol = STempSymbol
            | SVariable SType
            | SArray SType SSize
            | SConst SType Int
            | SFunction SType [SType]
            deriving (Show)

data SType = SInt | SChar | SVoid  deriving (Show)


type SSize = Int


data ExprValue = EStrictN Int
               | EVariable
               | ENot
               | EArray
               deriving (Show)

