{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Compiler where

import qualified Data.Map as M 
import Control.Monad.State.Lazy
import Data.Maybe (fromJust, isNothing)
import Parser
import Data.Binary (Word8)
import Data.Bits
import GHC.Float (castDoubleToWord64)
import qualified Data.ByteString as B
import Data.List (sortOn)
import qualified Data.Bifunctor as BF
import Typechecker 
import qualified Stack as S
import ScopeStack
import Token
import Types

data KeyType = AnonKey Int | NamedKey String deriving (Show, Ord, Eq)
type ConstTable = M.Map KeyType ([Word8], Int)
type ByteSeq = [Word8]


-- Instructions
opHalt :: Word8 
opHalt = 0x00

opConst :: Word8
opConst = 0x01

opMemSnum :: Word8
opMemSnum = 0x02

opMemSobj :: Word8
opMemSobj = 0x03

opAdd :: Word8 
opAdd = 0x04

opSub :: Word8
opSub = 0x05

opMult :: Word8

opMult = 0x06

opDiv :: Word8
opDiv = 0x07

opJmpAbs :: Word8 
opJmpAbs = 0x08

opJmpRel :: Word8
opJmpRel = 0x09

opCondJmpAbs :: Word8
opCondJmpAbs = 0x0A

opCondJmpRel :: Word8
opCondJmpRel = 0x0B

opLtCmp :: Word8
opLtCmp = 0x0C

opLteCmp :: Word8
opLteCmp = 0x0D

opEqCmp :: Word8
opEqCmp = 0x0E

opRet :: Word8
opRet = 0x0F

opLoadL :: Word8
opLoadL = 0x10

opLoadG :: Word8
opLoadG = 0x11

opStoreL :: Word8
opStoreL = 0x12

opStoreG :: Word8
opStoreG = 0x13

opCall :: Word8
opCall = 0x14

opMkClsr :: Word8
opMkClsr = 0x15

opMemSLnum :: Word8
opMemSLnum = 0x16

opMemSLobj :: Word8
opMemSLobj = 0x17

opLoadP :: Word8
opLoadP = 0x18

opJmpRelFalse :: Word8
opJmpRelFalse = 0x19

opLogOr :: Word8
opLogOr = 0x20

opLogAnd :: Word8
opLogAnd = 0x21

opCallNative :: Word8
opCallNative = 0x23

opLoadOuter :: Word8
opLoadOuter = 0x24

opStoreOuter :: Word8
opStoreOuter = 0x25

opJmpRelTrue :: Word8
opJmpRelTrue = 0x26

opIndex :: Word8
opIndex = 0x27 

opMkList :: Word8
opMkList = 0x28

opSetList :: Word8
opSetList = 0x29

opAppendList :: Word8
opAppendList = 0x30

opMkStruct :: Word8
opMkStruct = 0x31 

opAccessField :: Word8
opAccessField = 0x32

opSetField :: Word8
opSetField = 0x33

-- constant types 
numberConst :: Word8
numberConst = 0

stringConst :: Word8
stringConst = 1

closureConst :: Word8
closureConst = 2

listConst :: Word8
listConst = 3

charToWords8 :: Enum a => a -> [Word8]
charToWords8 c = if enumed == 0 then [0] else go enumed where
    enumed = fromEnum c
    go :: Int  -> [Word8]
    go 0 = []
    go i = fromIntegral (0xFF .&. i) : go (shift i (-8))

doubleToBytes :: Double -> [Word8]
doubleToBytes d = let
    bs = charToWords8 $ castDoubleToWord64 d
    in (map fromIntegral $ replicate (8 - length bs) 0) ++ bs

extendByte :: Enum a => a -> ByteSeq 
extendByte a = if fromEnum a <= 255 then 0 : charToWords8 a else charToWords8 a 


compileExpr :: Expr -> State (ConstTable, ScopeStack) ByteSeq

compileExpr (Literal tok) = do
    s <- gets $ fst
    case (tokType tok) of
        Number -> case M.lookup (NamedKey $ tokValue tok) s of
            Just (_, idx) -> return $ opConst : extendByte idx
            Nothing -> do
                let a = doubleToBytes $ read $ tokValue tok
                modify $ \(x, y) -> (M.insert (NamedKey $ tokValue tok) (numberConst : a, M.size x) x, y)
                return $ opConst : extendByte (M.size s)
        PString -> case M.lookup (NamedKey $ tokValue tok) s of
            Just (_, idx) -> return $ opConst : extendByte idx
            Nothing -> do
                let a = concatMap charToWords8 $ tokValue tok
                modify $ \(x, y) -> (M.insert (NamedKey $ tokValue tok) (stringConst : (extendByte $ length a + 1) ++ a ++ [0], M.size x) x, y)
                return $ opConst : extendByte (M.size s)
        Identifier -> do
            stack <- gets $ snd
            case lookupStackWithDepth (tokValue tok) stack of
                Just (a, d) -> if d == 0 then 
                    return $ opLoadL : extendByte (ref a)
                    else 
                        return $ opLoadOuter : (fromIntegral d) : extendByte (ref a)
                Nothing -> return []

        _ -> return []

compileExpr (FuncCall id args) = do
    syms <- gets $ S.peekTop . snd
    argsCode <- concat <$> traverse compileExpr args
    case (tokValue id) of
        "print" -> return $ argsCode ++ [opCallNative, 0]
        "time" -> return [opCallNative, 1]
        _ -> do
            stack <- gets $ snd 
            let (a, d) = fromJust $ lookupStackWithDepth (tokValue id) stack
            if d == 0 then
                return $ argsCode ++ opLoadL : extendByte (ref a) ++ [opCall] -- the issues are here
            else 
                return $ argsCode ++ opLoadOuter : fromIntegral d : extendByte (ref a) ++ [opCall]
compileExpr (Elif tok cond closure) = compileExpr (If tok cond closure [] Nothing) 

compileExpr (Else _ closure) = concat <$> traverse compileStatement (stmts closure)


-- for if statements: strip the statements from the closure
-- we don't want to treat if-elif-else expressions as function calls
compileExpr (If _ cond closure elifs elseStmt) = do
    cond' <- compileExpr cond 
    clsr' <- concat <$> traverse compileStatement (stmts closure) 
    elifs' <- concat <$> traverse compileExpr elifs
    else' <- case elseStmt of
        Just e -> compileExpr e 
        Nothing -> return [] 
    let clsrLen = extendByte $ length clsr'
    let elseLen = extendByte $ length else'
    return $ cond' ++ opJmpRelFalse : clsrLen ++ clsr' ++ cond' ++ opJmpRelTrue : elseLen ++ else'

compileExpr (List _ exprs) = do
    exprs' <- concat <$> traverse compileExpr exprs
    k <- gets $ M.size . fst
    let listLen = extendByte $ length exprs
    return $ exprs' ++ opMkList : listLen -- this imposes a limit on the max length of a defined array

compileExpr (ListIndex id idx) = do
    stack <- gets snd
    let (a, d) = fromJust $ lookupStackWithDepth (tokValue id) stack
    idx' <- compileExpr idx
    if d == 0 then
        return $ opLoadL : extendByte (ref a) ++ idx' ++ [opIndex]
    else
        return $ opLoadOuter : fromIntegral d : extendByte (ref a) ++ idx' ++ [opIndex]

compileExpr (Struct id fields) = do
    let indexedFields = zip fields $ reverse [0 .. length fields - 1] -- reverse because the stack 'reverses' the fields
    ws <- traverse compileField indexedFields
    return $ concatMap fst (sortOn snd ws) ++ [opMkStruct, (fromIntegral $ length fields)]
    where
        compileField = \case
            (ExprField expr, ord) -> do 
                e <- compileExpr expr
                return $ (e, ord)
            (KeyField tok expr, _) -> do
                e <- compileExpr expr
                stack <- gets snd
                let (StructSym struct fields _) = fromJust $ lookupStackTable (tokValue id) stack
                let key = fromJust $ M.lookup (tokValue tok) fields
                return $ (e, snd key)

compileExpr (StructField head fieldChain) = do
    stack <- gets snd
    let fieldNumList = go head fieldChain stack
    head' <- compileExpr $ Literal head
    return $ head' ++ fieldNumList
    where
        go :: Token -> [Token] -> ScopeStack -> [Word8]
        go struct (f:fs) stack = let
            -- fromJust is problematic, move these 3 lines to the top    
            sym = fromJust $ lookupStackTable (tokValue struct) stack
            (CustomType stName) = fromJust $ symType sym
            structSym = fromJust $ lookupStackTable stName stack
            field = fromJust $ M.lookup (tokValue f) $ fields structSym
            (CurryType [CustomType fieldName]) = fst field
            fieldTok = structName . fromJust $ lookupStackTable fieldName stack
            in case fs of
                [] -> [opAccessField, fromIntegral $ snd field]
                _ -> [opAccessField, fromIntegral $ snd field] ++ go fieldTok fs stack

    
    


-- binary ops
compileExpr (Add _ e1 e2) = liftA2 (++) (liftA2 (++) (compileExpr e1)  (compileExpr e2)) (pure [opAdd]) 
compileExpr (Sub _ e1 e2) = liftA2 (++) (liftA2 (++) (compileExpr e1)  (compileExpr e2)) (pure [opSub]) 
compileExpr (Mult _ e1 e2) = liftA2 (++) (liftA2 (++) (compileExpr e1)  (compileExpr e2)) (pure [opMult]) 
compileExpr (Div _ e1 e2) = liftA2 (++) (liftA2 (++) (compileExpr e1)  (compileExpr e2)) (pure [opDiv])
compileExpr (LogOr _ e1 e2) = liftA2 (++) (liftA2 (++) (compileExpr e1) (compileExpr e2)) (pure [opLogOr]) 
compileExpr (LogAnd _ e1 e2) = liftA2 (++) (liftA2 (++) (compileExpr e1) (compileExpr e2)) (pure [opLogAnd]) 
compileExpr (CmpBt _ e1 e2) = liftA2 (++) (liftA2 (++) (compileExpr e1) (compileExpr e2)) (pure [opLtCmp]) 
compileExpr (CmpBte _ e1 e2) = liftA2 (++)  (liftA2 (++) (compileExpr e1) (compileExpr e2)) (pure [opLteCmp]) 
compileExpr (CmpLt _ e1 e2) = liftA2 (++) (liftA2 (++) (compileExpr e2) (compileExpr e1)) (pure [opLtCmp]) 
compileExpr (CmpLte _ e1 e2) = liftA2 (++) (liftA2 (++) (compileExpr e2) (compileExpr e1)) (pure [opLteCmp]) 
compileExpr (CmpEq _ e1 e2) = liftA2 (++) (liftA2 (++) (compileExpr e1) (compileExpr e2)) (pure [opEqCmp]) 
compileExpr (CmpNeq _ e1 e2) = liftA2 (++) (liftA2 (++) (compileExpr e1) (compileExpr e2)) (pure [opLogOr]) 


-- closure-based expressions
-- closure data: [<closure_type>, <arity>, <locals_size>, <insts_size>, <num_keys>, <insts>, <consts_size>, <consts>

compileExpr clsr@(Closure addr stmts syms) = do
    modify $ \s -> (fst s, S.push syms $ snd s)
    s <- gets $ fst 
    stack <- gets $ snd
    let (a, s') = runState (concat <$> (traverse compileStatement stmts)) (M.empty, stack)
    let locsLen = extendByte $ M.size syms
    let bytesLen = extendByte $ length a + 1
    let consts = concatMap (fst . snd) $ sortOn (snd . snd) $ M.toList $ fst s'
    let constsLen = extendByte $ M.size $ fst s'
    let num_keys = extendByte $ length $ filter (\x -> isSymbol x && isOuterValue x) $ M.elems syms
    let clsrData = closureConst : 0 : locsLen ++ bytesLen ++ num_keys ++ a ++ [opRet] ++ constsLen ++ consts 
    let idx = extendByte $ M.size s
    modify $ \(x,y) -> (M.insert (AnonKey $ M.size s)  (clsrData, M.size x) x, snd $ S.pop y)
    return $ opConst : idx ++ [opCall]

runCompileExpr expr = do
    s <- get
    let (a, s) = runState (compileExpr expr) s
    return (a,s)



-- statements


compileStatement :: Statement -> State (ConstTable, ScopeStack) ByteSeq
compileStatement (Assignment var expr) = case var of
    Literal tok -> do
        tup  <- gets $ fromJust . lookupStackWithDepth (tokValue tok) . snd
        let (sym, d) = tup
        res <- case expr of 
            clsr@(Closure addr _ csyms) -> do
                let arity :: Word8 = fromIntegral $ length $ params sym
                s <- get
                let (a, s') = runState (compileExpr expr) (M.empty, snd s)
                put $ (fst s, snd s')
                let (x:_:ys) = concatMap (fst . snd) $ M.toList $ fst s'
                let varName = tokValue tok
                newS <- get
                let newIdx = M.size $ fst newS
                let newInsts = opConst : extendByte newIdx
                modify $ \(a, b) -> (M.insert ( NamedKey varName ) ((x:arity:ys), M.size $ fst newS) a, b)
                return $ newInsts


            _ -> compileExpr expr
         

        if d == 0 then
            if isSymbol sym && isOuterValue sym then do
                let idx = extendByte $ ref sym
                return $ res ++ opStoreL : idx ++ opLoadL : idx ++ opStoreOuter : 0 : idx
            else
                return $ res ++ opStoreL : extendByte (ref sym)
        else do 
            let idx = extendByte (ref sym)
            return $ res ++ opStoreOuter : (fromIntegral d) : idx

    ListIndex idTok idx -> do
        tup <- gets $ fromJust . lookupStackWithDepth (tokValue idTok) . snd
        let (sym, d) = tup
        idx' <- compileExpr idx
        expr' <- compileExpr expr
        if d == 0 then
            return $ opLoadL : extendByte (ref sym) ++ idx' ++ expr' ++ [opSetList]
        else
            return $ opLoadOuter : (fromIntegral d) : extendByte (ref sym) ++ idx' ++ expr' ++ [opSetList]

    StructField head fieldChain -> do
        stack <- gets snd
        head' <- compileExpr $ Literal head
        expr' <- compileExpr expr
        return $ expr' ++ head' ++ go head fieldChain stack
        where
            go :: Token -> [Token] -> ScopeStack -> [Word8]
            go struct (f:fs) stack = let
                sym = fromJust $ lookupStackTable (tokValue struct) stack
                (CustomType stName) = fromJust $ symType sym
                structSym = fromJust $ lookupStackTable stName stack
                field = fromJust $ M.lookup (tokValue f) $ fields structSym
                (CurryType [CustomType fieldName]) = fst field
                fieldTok = structName . fromJust $ lookupStackTable fieldName stack
                in case fs of
                    [] -> [opSetField, fromIntegral $ snd field]
                    _ -> [opAccessField, fromIntegral $ snd field] ++ go fieldTok fs stack

    List t items -> case expr of
        List t' items' -> go items items'
        nonList -> return [] -- this should never happen given the correctness of the typechecker
        where
            go [] _ = return []
            go _ [] = return []
            go (x:xs) (y:ys) = do 
                ws <- compileStatement $ Assignment x y
                rest <- go xs ys
                return $ ws ++ rest





compileStatement (ExprStmt expr) = compileExpr expr 
compileStatement _ = return []


