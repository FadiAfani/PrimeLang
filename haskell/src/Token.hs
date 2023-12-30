module Token where

data TokenType = PString 
    | Number
    | LogTrue
    | LogFalse
    | Identifier
    | Lesser
    | Bigger
    | LesserEq
    | BiggerEq
    | LogicalAnd
    | LogicalOr
    | Equal
    | NotEqual
    | Negation
    | Plus
    | Minus
    | Star
    | RngOp
    | InOp
    | Keyword
    | BackSlash deriving (Show, Eq)


data Token = Token {tokPos :: (Int, Int), tokValue :: String, tokType :: TokenType} deriving (Show, Eq)
