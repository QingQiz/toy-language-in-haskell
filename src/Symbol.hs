module Symbol where

import Data.Map


----                   name   type
type SymbolTable = Map String Symbol


data Symbol = SReserveSymbol
            | STempSymbol String Symbol
            | SVariable SType
            | SArray SType SSize
            | SConst SType Int
            | SFunction SType [SType]
            deriving (Show)

data SType = SInt | SChar | SVoid  deriving (Show, Eq)

type SSize = Int

empty_st :: Map String Symbol
empty_st = fromList [
    ("if",     SReserveSymbol), ("else",     SReserveSymbol),
    ("for",    SReserveSymbol), ("do",       SReserveSymbol), ("while", SReserveSymbol),
    ("break",  SReserveSymbol), ("continue", SReserveSymbol),
    ("printf", SReserveSymbol), ("scanf",    SReserveSymbol), ("read",  SReserveSymbol), ("print", SReserveSymbol),
    ("const",  SReserveSymbol), ("char",     SReserveSymbol), ("int",   SReserveSymbol),
    ("void",   SReserveSymbol), ("return",   SReserveSymbol)]
