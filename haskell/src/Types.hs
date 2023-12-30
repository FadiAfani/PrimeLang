module Types where

data PrimeType = NumberType
    | StringType
    | BoolType 
    | CustomType String
    | ListType PrimeType
    | CurryType [PrimeType] deriving (Show, Eq)


