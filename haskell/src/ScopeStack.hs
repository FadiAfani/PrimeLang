module ScopeStack where

import Data.Map as M
import qualified Stack as S
import Token
import Types

type Identifier = String
data Symbol = Symbol {params :: [Token], symType :: Maybe PrimeType, ref :: Int, isOuterValue :: Bool} 
    | StructSym { structName :: Token, fields :: M.Map String PrimeType } deriving (Show, Eq)
type SymbolTable = M.Map Identifier Symbol 
type ScopeStack = S.Stack SymbolTable

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

