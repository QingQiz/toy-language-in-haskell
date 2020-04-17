module TransAst where

import Data.Map


----                   name   type
type SymbolTable = Map String Symbol


data Symbol = STempSymbol
            | SVariable SType
            | SArray SType SSize
            | SConst SType
            | SFunction SType [SType]
            deriving (Show)

data SType = SInt | SChar | SVoid  deriving (Show)

type SSize = Int


