module Symbol where

import Data.Map


----                   name   type
type SymbolTable = Map String Symbol


data Symbol = SReserveSymbol
            | STempSymbol
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


empty_st :: Map String Symbol
empty_st = fromList [
    ("if",     SReserveSymbol), ("else",   SReserveSymbol),
    ("for",    SReserveSymbol), ("do",     SReserveSymbol), ("while", SReserveSymbol),
    ("printf", SReserveSymbol), ("scanf",  SReserveSymbol),
    ("const",  SReserveSymbol), ("char",   SReserveSymbol), ("int",   SReserveSymbol),
    ("void",   SReserveSymbol), ("return", SReserveSymbol)]

