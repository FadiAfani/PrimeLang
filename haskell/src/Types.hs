module Types where

import Token
import qualified Data.Map as M

data SubType = RecordType String | EnumType String deriving (Show, Eq)

data PrimeType = NumberType
    | StringType
    | BoolType 
    | CustomType String
    | ListType PrimeType
    | AliasType String
    | CurryType [PrimeType] deriving (Show, Eq)

-- type <name> = Tree { left: Tree; root: number; right: Tree } | Empty
-- type <name><T> = Tree { left: Tree; root: T; right: Tree } | Empty
