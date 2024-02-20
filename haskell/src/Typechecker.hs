{-# LANGUAGE LambdaCase #-}

module Typechecker where

import Parser
import Control.Monad.State.Lazy
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing, isJust, catMaybes)
import Control.Applicative
import GHC.List (unsnoc)
import Data.List (find)
import Types
import ScopeStack 
import Token
import qualified Stack as S

data CompilationError = CompilationError {pos :: (Int, Int), msg :: String} deriving Show


getExprType :: Expr -> StateT ScopeStack (Either CompilationError) (Maybe PrimeType)
getExprType (Add tok a b) = do
    typeA <- getExprType a
    typeB <- getExprType b 
    case (typeA , typeB) of
        (Just NumberType, Just NumberType) -> return $ Just NumberType
        (Just StringType, Just StringType) -> return $ Just StringType
        (Nothing, _) -> return Nothing
        (_, Nothing) -> return Nothing
        _ -> reportError $ CompilationError (tokPos tok) $ "Incompatible Types: '+' is not supported between types " ++ show typeA ++ " and " ++ show typeB

getExprType (Sub tok a b) = do
    typeA <- getExprType a
    typeB <- getExprType b 
    case (typeA , typeB) of
        (Just NumberType, Just NumberType) -> return $ Just NumberType
        (Nothing, _) -> return Nothing
        (_, Nothing) -> return Nothing 
        _ -> reportError $ CompilationError (tokPos tok) $ "Incompatible Types: '-' is not supported between types " ++ show typeA ++ " and " ++ show typeB

getExprType (Mult tok a b) = do
    typeA <- getExprType a
    typeB <- getExprType b 
    case (typeA , typeB) of
        (Just NumberType, _) -> return $ Just NumberType
        (_, Just NumberType) -> return $ Just NumberType
        (Nothing, _) -> return Nothing
        (_, Nothing) -> return Nothing 
        _ -> reportError $ CompilationError (tokPos tok) $ "Incompatible Types: '*' is not supported between types " ++ show typeA ++ " and " ++ show typeB


getExprType (Div tok a b) = do
    typeA <- getExprType a
    typeB <- getExprType b 
    case (typeA , typeB) of
        (Just NumberType, Just NumberType) -> return $ Just NumberType
        (Nothing, _) -> return Nothing
        (_, Nothing) -> return Nothing 
        _ -> reportError $ CompilationError (tokPos tok) $ "Incompatible Types: '/' is not supported between types " ++ show typeA ++ " and " ++ show typeB

getExprType (CmpBt tok a b) = do
    typeA <- getExprType a
    typeB <- getExprType b 
    case (typeA , typeB) of
        (Just NumberType, Just NumberType) -> return $ Just NumberType
        (Nothing, _) -> return Nothing
        (_, Nothing) -> return Nothing 
        _ -> reportError $ CompilationError (tokPos tok) $ "Incompatible Types: '>' is not supported between types " ++ show typeA ++ " and " ++ show typeB


getExprType (CmpBte tok a b) = do
    typeA <- getExprType a
    typeB <- getExprType b 
    case (typeA , typeB) of
        (Just NumberType, Just NumberType) -> return $ Just NumberType
        (Nothing, _) -> return Nothing
        (_, Nothing) -> return Nothing 
        _ -> reportError $ CompilationError (tokPos tok) $ "Incompatible Types: '>=' is not supported between types " ++ show typeA ++ " and " ++ show typeB

getExprType (CmpLt tok a b) = do
    typeA <- getExprType a
    typeB <- getExprType b 
    case (typeA , typeB) of
        (Just NumberType, Just NumberType) -> return $ Just NumberType
        (Nothing, _) -> return Nothing
        (_, Nothing) -> return Nothing 
        _ -> reportError $ CompilationError (tokPos tok) $ "Incompatible Types: '<' is not supported between types " ++ show typeA ++ " and " ++ show typeB

getExprType (CmpLte tok a b) = do
    typeA <- getExprType a
    typeB <- getExprType b 
    case (typeA , typeB) of
        (Just NumberType, Just NumberType) -> return $ Just NumberType
        (Nothing, _) -> return Nothing
        (_, Nothing) -> return Nothing 
        _ -> reportError $ CompilationError (tokPos tok) $ "Incompatible Types: '<=' is not supported between types " ++ show typeA ++ " and " ++ show typeB

getExprType (CmpNeq tok a b) = do
    typeA <- getExprType a
    typeB <- getExprType b 
    case (typeA , typeB) of
        (Just NumberType, Just NumberType) -> return $ Just NumberType
        (Just StringType, Just StringType) -> return $ Just StringType
        (Nothing, _) -> return Nothing
        (_, Nothing) -> return Nothing 
        _ -> reportError $ CompilationError (tokPos tok) $ "Incompatible Types: '!=' is not supported between types " ++ show typeA ++ " and " ++ show typeB

getExprType (CmpEq tok a b) = do
    typeA <- getExprType a
    typeB <- getExprType b 
    case (typeA , typeB) of
        (Just NumberType, Just NumberType) -> return $ Just NumberType
        (Just StringType, Just StringType) -> return $ Just StringType
        (Nothing, _) -> return Nothing
        (_, Nothing) -> return Nothing 
        _ -> reportError $ CompilationError (tokPos tok) $ "Incompatible Types: '==' is not supported between types " ++ show typeA ++ " and " ++ show typeB


getExprType (Range tok a b) = do
    typeA <- getExprType a
    typeB <- getExprType b 
    case (typeA , typeB) of
        (Just NumberType, Just NumberType) -> return $ Just NumberType
        (Nothing, _) -> return Nothing
        (_, Nothing) -> return Nothing 
        _ -> reportError $ CompilationError (tokPos tok) $ "Incompatible Types: '..' is not supported between types " ++ show typeA ++ " and " ++ show typeB

getExprType (Closure _ stmts syms') = do
    modify $ S.push syms'
    case unsnoc stmts of
        Just (_, x) -> case x of
            ExprStmt expr -> do 
                t <- getExprType expr
                modify $ snd . S.pop 
                return t
            _ -> reportError $ CompilationError (0,0) $ "Syntax Error: closures must end with an expression"
        Nothing -> reportError $ CompilationError (0,0) $ "Syntax Error: closures must end with an expression"

getExprType (If tok _ closure elifs elseStmt) = do
    closureType <- getExprType closure 
    elifsTypes <- traverse getExprType elifs  
    case elseStmt of
            Just e -> do
                elseType <- getExprType e 
                if and (map ( == closureType ) (elseType : elifsTypes)) then
                    return $ closureType
                    else
                        reportError $ CompilationError (tokPos tok) $ "Incompatible Types: all final expression types in an if expression must match"
            Nothing -> if and (map ( == closureType ) elifsTypes) then
                return $ closureType
                else
                    reportError $ CompilationError (tokPos tok) $ "Incompatible Types: all final expression types in an if expression must match"

getExprType (Elif _ _ closure) = getExprType closure 
getExprType (Else _ closure) = getExprType closure 
getExprType (For _ _ closure) = getExprType closure 
getExprType (While _ closure) = getExprType closure 
getExprType (Negative _ expr) = getExprType expr 

getExprType (Literal tok) = case tokType tok of
    Identifier -> do
        stack <- get  
        case lookupStackTable (tokValue tok) stack of
            Just a -> return $ symType a 
            Nothing -> reportError $ CompilationError (tokPos tok) $ "Undefined Symbol: '" ++ (tokValue tok) ++ "' is not defined" 
    Number -> return $ Just NumberType
    PString -> return $ Just StringType
    _ -> reportError $ CompilationError (tokPos tok) $ "Syntax Error: this symbol cannot be used as an identifier"

getExprType (FuncCall tok args) = do
    stack <- get
    case lookupStackTable (tokValue tok) stack of
        Just a -> do
            case symType a of
                Just (CurryType []) -> return Nothing -- eventually this will be changed to a unit type 
                Just (CurryType xs) -> return $ Just $ last xs -- return type
                Just t -> return $ Just t
                _ -> return Nothing
        Nothing -> case (tokValue tok) of 
            "print" -> return Nothing
            "time" -> return Nothing
            _ -> reportError $ CompilationError (tokPos tok) $ "Undefined Symbol: '" ++ (tokValue tok) ++ "' is not defined"

getExprType (List listType items) = do
    if isNothing listType then
        case items of
            [] -> return Nothing
            (x:xs) -> do 
               t <- getExprType x
               ts <- traverse getExprType xs
               if and $ map ( == t) ts then
                   return t
               else
                   reportError $ CompilationError (1,1) $ "Type Error: list items must of the type " ++ show t    
    else
        return listType

getExprType (ListIndex listId idx) = do
   stack <- get
   case lookupStackTable (tokValue listId) stack of 
       Just a -> return $ symType a
       Nothing -> return Nothing

getExprType (Struct id fields) = 
    if (isJust $ find isKeyField fields) && (isJust $ find isExprField fields) then 
        reportError $ CompilationError (tokPos id) $ "Syntax Error: structs cannot have a mixture of key/expression and expression fields"
    else
        return $ Just $ CustomType (tokValue id)
    where
        isKeyField = \case
            KeyField _ _ -> True
            _ -> False
        isExprField = \case
            ExprField _ -> True
            _ -> False

getExprType (StructField head fieldChain) = do
    stack <- get
    case runStateT (go head fieldChain) stack of
        Right (a, s) -> return $ Just a
        Left err -> reportError err
    where
        go cur (f:fs) = do
            stack <- get
            let varSym = fromJust $ lookupStackTable (tokValue cur) stack
            let (CustomType structName) = fromJust $ symType varSym
            let structSym = fromJust $ lookupStackTable structName stack

            -- this causes a selector error (fields)
            case M.lookup (tokValue f) (fields structSym) of
                Just t -> case fs of
                    [] -> return $ fst t
                    _ -> go f fs
                Nothing -> reportError $ CompilationError (tokPos f) $ "Invalid Field: this struct does not have a field " ++ (tokValue f)


getExprType _ = return $ Just NumberType 

reportError :: CompilationError -> StateT ScopeStack (Either CompilationError) a 
reportError err = StateT $ \_ -> Left err

updateExpr :: Expr -> StateT ScopeStack (Either CompilationError) Expr
updateExpr = \case
    Closure addr stmts syms -> do
        modify $ S.push syms 
        stmts' <- traverse updateStatement stmts
        (top, rest) <- gets $ \s -> case S.pop s of
            (Just t, r) -> (t, r)
            (Nothing, r) -> (M.empty, r)
        put rest
        return $ Closure addr stmts' top
    If tok cond block elifs melse -> do
        s <- get
        cond' <- updateExpr cond 
        block' <- updateExpr block
        elifs' <- traverse updateExpr elifs
        case melse of
            Just e -> do
                e' <- updateExpr e
                return $ If tok cond' block' elifs' $ Just e'
            Nothing -> return $ If tok cond' block' elifs' Nothing

    Elif tok cond closure -> do
        s <- get 
        cond' <- updateExpr cond
        closure' <- updateExpr closure
        return $ Elif tok cond' closure'

    Else tok closure -> do
        closure' <- updateExpr closure
        return $ Else tok closure'

    For tok cond closure -> do
        cond' <- updateExpr cond
        closure' <- updateExpr closure
        return $ For tok cond' closure'

    While cond closure -> do
        cond' <- updateExpr cond
        closure' <- updateExpr closure
        return $ While cond' closure'

    Literal tok -> case (tokType tok) of
        Identifier -> do
            s <- get
            case lookupTop (tokValue tok) s of 
                Just _ -> return $ Literal tok
                Nothing -> do
                    modify $ \s -> adjustStackTable (tokValue tok) (\a -> a {isOuterValue = True}) s
                    return $ Literal tok
        _ -> return $ Literal tok

    Mult tok a b -> do
        a' <- updateExpr a
        b' <- updateExpr b
        return $ Mult tok a' b'

    Add tok a b -> do
        a' <- updateExpr a
        b' <- updateExpr b
        return $ Add tok a' b'
    
    Div tok a b -> do
        a' <- updateExpr a
        b' <- updateExpr b
        return $ Div tok a' b'

    Sub tok a b -> do
        a' <- updateExpr a
        b' <- updateExpr b
        return $ Sub tok a' b'

    LogAnd tok a b -> do
        a' <- updateExpr a
        b' <- updateExpr b
        return $ LogAnd tok a' b'

    LogOr tok a b -> do
        a' <- updateExpr a
        b' <- updateExpr b
        return $ LogOr tok a' b'

    FuncCall tok args -> do
        args' <- traverse updateExpr args
        argTypes <- traverse getExprType args
        let argTypes' = map fromJust argTypes
        modify $ adjustStackTable (tokValue tok) (\a -> a { symType = Just $ CurryType argTypes' })
        s <- get
        case lookupTop (tokValue tok) s of 
            Just a -> if isJust (symType a) then
                return $ FuncCall tok args'
                else
                    return $ FuncCall tok args
            Nothing -> do 
                modify $ \s -> adjustStackTable (tokValue tok) (\a -> a {isOuterValue = True}) s
                return $ FuncCall tok args'
        return $ FuncCall tok args'

    expr@(List listType exprs) -> case listType of
        Nothing -> do
            t <- getExprType expr
            exprs' <- traverse updateExpr exprs
            return $ List t exprs'
        e -> do
            exprs' <- traverse updateExpr exprs
            return $ List listType exprs'

    Struct tok args -> do
        args' <- traverse updateArg args
        return $ Struct tok args'
        where
            updateArg = \case
                ExprField expr -> ExprField <$> updateExpr expr
                KeyField tok expr -> KeyField tok <$> updateExpr expr


    e -> return e
updateStatement :: Statement -> StateT ScopeStack (Either CompilationError) Statement
updateStatement (Assignment var expr) = case var of
    Literal tok -> do
        expr' <- updateExpr expr
        t <- getExprType expr'
        modify $ \s -> adjustStackTable (tokValue tok) (\a -> a {symType = t}) s 
        return $ Assignment var expr'

    lp@(List mtype vars) -> do 
        expr' <- updateExpr expr
        case expr' of
            lc@(List mt exprItems) -> do
                -- list should now be of a single uniform type (after updateExpr)
                go vars exprItems
                return $ Assignment lp lc
            nonListExpr -> reportError $ CompilationError (1,1) "Pattern Mismatch: pattern does not match the expression"
            where
                go :: [Expr] -> [Expr] -> StateT ScopeStack (Either CompilationError) [Statement]
                go [] [] = return []
                go (x:_) [] = reportError $ CompilationError (1,1) "Pattern Mismatch: pattern does not match the expression"
                go [] (y:_) = reportError $ CompilationError (1,1) "Pattern Mismatch: pattern does not match the expression"
                go (x:xs) (y:ys) = case x of
                    Literal x' -> do 
                        ymt <- getExprType y
                        len <- gets $ M.size . fromJust . S.peekTop
                        modify $ insertTop (tokValue x') (Symbol [] ymt len False)
                        go xs ys
                    nonLit -> reportError $ CompilationError (1,1) "Type Error: deconstructors may only hold identifier literals"

    e -> do
        left <- updateExpr e
        right <- updateExpr expr
        return $ Assignment left right

updateStatement (ExprStmt expr) = ExprStmt <$> updateExpr expr
updateStatement (StructDecl tok) = return $ StructDecl tok 

-- this operation is expensive but i don't care 
-- (could've had the types registered as tokens in the symbol table for O(log(n)) lookup)
updateStatement decl@(DataTypeDecl name gens subs) = do
    let typeId = tokValue name 
    pSubs <- traverse subToPrime subs
    modify $ insertTop (tokValue name) $ TupleTypeSym name gens $ Just $ CustomType typeId
    return decl 
    where
        subToPrime = \case
           TupleSubType con fs ->  do
               let typeId = tokValue name
               s <- get
               let fs' = zip fs $ map ( isTypeRegistered s . tokValue) fs
               let badType = find ( ( == False) . snd) fs'
               pTypes <- case badType of
                   Just (t, _) -> reportError $ CompilationError (tokPos t) $ "Unknown Type: " ++ tokValue t ++ " is not a known type"
                   goodType -> return $ map (getRegisteredType s . tokValue . fst) fs'
               modify $ insertTop (tokValue con) ( EnumSym con (catMaybes pTypes) $ Just $ CustomType typeId )
           EnumType tok -> do 
               let typeId = tokValue name
               modify $ insertTop (tokValue tok) (EnumSym tok [] $ Just $ CustomType typeId)

updateAST :: [Statement] -> StateT ScopeStack (Either CompilationError) [Statement]
updateAST = traverse updateStatement
