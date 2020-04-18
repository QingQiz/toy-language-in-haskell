module TransAst where

import Data.Map


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
               deriving (Show)

