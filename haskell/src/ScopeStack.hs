{-# LANGUAGE LambdaCase #-}

module ScopeStack where

import qualified Data.Map as M
import qualified Stack as S
import Token
import Types
import Data.Maybe (catMaybes, isJust)
import Data.List (find)

type Identifier = String
type Order = Int
data Symbol = Symbol {params :: [Token], symType :: Maybe PrimeType, ref :: Int, isOuterValue :: Bool}
    -- symType below is wrapped in Maybe to avoid upsetting the compiler during semantic analysis phase but otherwise it is not needed
    | StructSym { structName :: Token, fields :: M.Map String (PrimeType, Order), symType :: Maybe PrimeType } 
    | TupleTypeSym { typeName :: Token, generics :: [Token], symType :: Maybe PrimeType } 
    | EnumSym { enumName :: Token, subTypes :: [PrimeType],  symType :: Maybe PrimeType } deriving (Show, Eq)

type SymbolTable = M.Map Identifier Symbol 
type ScopeStack = S.Stack SymbolTable

isSymbol :: Symbol -> Bool
isSymbol = \case
    Symbol {} -> True
    _ -> False

-- returns the list of all user-defined types
getRegisteredTypes :: S.Stack SymbolTable -> [PrimeType]
getRegisteredTypes = catMaybes . (map (toCustomType . snd) ) . toList
    where
        toCustomType :: Symbol -> Maybe PrimeType
        toCustomType = \case
            StructSym { symType = t } -> t
            TupleTypeSym {symType = t } -> t
            _ -> Nothing

isTypeRegistered :: S.Stack SymbolTable -> String -> Bool
isTypeRegistered stack = \case
    "number" -> True
    "string" -> True
    "bool" -> True
    t -> isJust $ find ( == CustomType t) $ getRegisteredTypes stack

getRegisteredType :: S.Stack SymbolTable -> String -> Maybe PrimeType
getRegisteredType stack = \case
    "number" -> Just NumberType
    "string" -> Just StringType
    "bool" -> Just BoolType
    t -> if isTypeRegistered stack t then Just (CustomType t) else Nothing

lookupStackTable :: Identifier -> S.Stack SymbolTable -> Maybe Symbol
lookupStackTable id stack = go 0 where
    go i = case S.peek i stack of
        Just table -> case M.lookup id table of
            Just sym -> Just sym
            Nothing -> go $ i + 1
        Nothing -> Nothing

lookupStackWithDepth :: Identifier -> S.Stack SymbolTable -> Maybe (Symbol, Int)
lookupStackWithDepth id stack = go 0 where
    go i = case S.peek i stack of
        Just table -> case M.lookup id table of
            Just sym -> Just $ (sym, i)
            Nothing -> go $ i + 1
        Nothing -> Nothing
adjustStackTable :: Identifier -> (Symbol -> Symbol) -> S.Stack SymbolTable -> S.Stack SymbolTable
adjustStackTable id f stack = go 0 where
    go :: Int -> S.Stack SymbolTable
    go i = case S.peek i stack of
        Just table -> if M.member id table 
        then 
            S.adjustStack i (M.adjust f id) stack 
        else
            go $ i + 1
        Nothing -> stack

lookupTop :: Identifier -> ScopeStack -> Maybe Symbol
lookupTop id stack = case S.peekTop stack of
    Just t -> M.lookup id t
    Nothing -> Nothing

insertTop :: Identifier -> Symbol -> ScopeStack -> ScopeStack
insertTop id sym stack = case S.pop stack of
    (Just top, rest) -> flip S.push stack $ M.insert id sym top
    _ -> stack

toList :: S.Stack SymbolTable -> [(Identifier, Symbol)]
toList = concatMap M.toList 

