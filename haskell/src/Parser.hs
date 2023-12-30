{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Parser where

import Control.Monad.State.Lazy
import qualified Data.Map as M
import Control.Applicative
import Control.Monad (MonadPlus (..), MonadFail)
import Data.Maybe (fromMaybe, isNothing, fromJust)
import Data.List (foldl')
import Stack as S
import ScopeStack
import Token
import Types

data ParserError = ParserError {pos :: (Int, Int), msg :: String}
data ParserState = ParserState {curPos :: (Int, Int), input :: String, scopeStack :: S.Stack SymbolTable} deriving (Show, Eq)
data Expr = Add Token Expr Expr 
    | Sub Token Expr Expr 
    | Div Token Expr Expr 
    | Mult Token Expr Expr
    | LogOr Token Expr Expr
    | LogAnd Token Expr Expr 
    | CmpBt Token Expr Expr
    | CmpBte Token Expr Expr
    | CmpLt Token Expr Expr
    | CmpLte Token Expr Expr
    | CmpEq Token Expr Expr
    | CmpNeq Token Expr Expr
    | Neg Token Expr Expr
    | Range Token Expr Expr
    | In Token Token Expr
    | Negative Token Expr
    | While {cond :: Expr, closure :: Expr}
    | Closure {clsrAddr :: String, stmts :: [Statement], blockSyms :: SymbolTable}
    | FuncCall {funcId :: Token, args :: [Expr]}
    | Literal Token 
    | If { token :: Token, 
        cond :: Expr, 
        closure :: Expr, 
        elifs :: [Expr], 
        elseStmt :: Maybe Expr}
    | Elif {token :: Token, cond :: Expr, closure :: Expr}
    | Else {token :: Token, closure :: Expr}
    | List { listType :: Maybe PrimeType, listItems :: [Expr]}
    | ListIndex { listId :: Token, index :: Expr }
    | For {token :: Token, cond :: Expr, closure :: Expr} deriving (Show, Eq)

data Statement = Assignment {var :: Expr, assigned :: Expr}
    | Def { defId :: Token, defType :: PrimeType}
    | DataType { inputTypes :: [Token], constructors :: [Statement] }
    | ExprStmt Expr deriving (Show, Eq)

type Parser a = StateT ParserState (Either ParserError) a

instance Show ParserError where
    show :: ParserError -> String
    show (ParserError pos msg) = show (fst pos) ++ ":" ++ show (snd pos) ++ ": " ++ msg


instance MonadFail (Either ParserError) where
    fail = Left . ParserError (1,1)

instance Alternative (Either ParserError) where
    empty :: Either ParserError a
    empty = Left $ ParserError (1,1) ""

    (<|>) :: Either ParserError a -> Either ParserError a -> Either ParserError a
    Left a <|> rb = rb
    la <|> _ = la


instance MonadPlus (Either ParserError) where
    mzero :: Either ParserError a
    mzero = empty
    
    mplus :: Either ParserError a -> Either ParserError a -> Either ParserError a
    mplus = (<|>)
    

runParser :: Parser a -> ParserState -> Either ParserError (a, ParserState)
runParser = runStateT

evalParser :: Parser a -> ParserState -> Either ParserError a
evalParser = evalStateT

consume :: String -> Parser ()
consume s = do
    inp <- gets input
    go s inp
    where
        go (x:xs) (y:ys) = if x == y then go xs ys else return ()
        go [] ys = modify $ \s -> s {input = ys}
        go _ [] = makeFailedParser $ "failure in parsing a " ++ s


makeFailedParser :: String -> Parser a
makeFailedParser s = StateT $ Left . flip ParserError s . curPos   

makeTokenParser :: TokenType -> String -> Parser Token
makeTokenParser t s = do
    pos <- gets curPos
    return $ Token pos s t

makeStringToken = makeTokenParser PString
makeLogAndToken = makeTokenParser LogFalse
makeLogTrueToken = makeTokenParser LogTrue
makeNumberToken = makeTokenParser Number

parseChar :: Char -> Parser Char
parseChar c = do
    s <- gets input
    case s of
        (x : xs) -> if x /= c then 
            makeFailedParser $ "Unexpected Symbol: expected " ++ c : " and got " ++ x : " instead" 
        else do
            (row, col) <- gets curPos
            case c of
                '\n' -> modify $ \s -> s {curPos = (row + 1, 1), input = xs}
                '\t' -> modify $ \s -> s {curPos = (row, col + 4), input = xs}
                _ -> modify $ \s -> s {curPos = (row, col + 1), input = xs}
            return c
        _ -> makeFailedParser $ "Unexpected Symbol: expected " ++ c : " and got nothing instead" 

parseString :: String -> Parser String
parseString = traverse parseChar

parseKeyword :: Parser String
parseKeyword = some $ asum $ map parseChar $ ['a' .. 'z'] ++ ['A' .. 'Z']

-- <literal> := <groupedExpr> | <Identifier> | <primitive>

parseDigit :: Parser Char
parseDigit = asum $ map parseChar ['0' .. '9']

parseRowSpace :: Parser Char
parseRowSpace = parseChar '\t' <|> parseChar ' '

parseColSpace :: Parser Char
parseColSpace = parseChar '\n'

parseWhiteSpace :: Parser Char
parseWhiteSpace = parseRowSpace <|> parseColSpace


parseRawToken :: TokenType -> String -> Parser Token
parseRawToken t s = do
    parseString s
    pos <- gets curPos
    return $ Token pos s t
parseNumberToken :: Parser Token
parseNumberToken = do
    d <- parseDigit
    ds <- many parseDigit
    frac <- optional $ parseChar '.' *> some parseDigit
    pos <- gets curPos
    case frac of
        Just f -> makeTokenParser Number $ d : ds ++ '.' : f 
        Nothing -> makeTokenParser Number $ d : ds

parseStringToken :: Parser Token 
parseStringToken = do
    parseChar '"'
    str <- gets $ takeWhile ( /= '"') . input
    str' <- parseString str
    parseChar '"'
    makeTokenParser PString str'

parseTrueToken :: Parser Token
parseTrueToken = do 
    parseString "true"
    makeTokenParser LogTrue "true"

parseFalseToken :: Parser Token
parseFalseToken = do
    parseString "false"
    makeTokenParser LogFalse "false"

parseIdentifierToken :: Parser Token
parseIdentifierToken = do
    x <- asum $ map parseChar $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']
    xs <- many $ asum $ map parseChar $ '_' : ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']
    makeTokenParser Identifier $ x : xs

parseFuncCall :: Parser Expr
parseFuncCall = do
    id <- parseIdentifierToken <* parseChar '(' <* many parseWhiteSpace
    args <- optional $ do
        xs <- many $ parseExpr <* many parseWhiteSpace <* parseChar ','
        many parseWhiteSpace
        x <- parseExpr <* many parseRowSpace
        return $ xs ++ [x]
    parseChar ')'
    return $ FuncCall id $ fromMaybe [] args

parseList :: Parser Expr
parseList = do
    parseChar '[' <* many parseWhiteSpace
    fstExpr <- parseExpr
    rest <- many $ parseChar ',' <* many parseWhiteSpace *> parseExpr
    parseChar ']'
    return $ List Nothing $ fstExpr : rest

parseListIndex :: Parser Expr
parseListIndex = do
    id <- parseIdentifierToken <* many parseRowSpace 
    parseChar '[' <* many parseWhiteSpace
    idx <- parseExpr <* many parseWhiteSpace
    parseChar ']'
    return $ ListIndex id idx

parseLiteral :: Parser Expr
parseLiteral = Literal <$> (parseNumberToken
    <|> parseStringToken
    <|> parseTrueToken
    <|> parseFalseToken
    <|> parseIdentifierToken
    )

parsePrimary :: Parser Expr
parsePrimary = parseGroupedExpr <|> parseFuncCall <|> parseList <|> parseListIndex <|> parseLiteral

parseFactor :: Parser Expr 
parseFactor = do
    a <- parsePrimary 
    go a 
    where
        go start = do
            many parseRowSpace 
            mop <- optional $ (parseRawToken Star "*") <|> (parseRawToken BackSlash "/")
            case mop of
                Just op -> do
                    many parseRowSpace
                    b <- parsePrimary <|> makeFailedParser "Fail"
                    case (tokValue op) of
                        "*" -> go $ Mult op start b
                        "/" -> go $ Div op start b
                        _ -> go $ Add op start b
                Nothing -> return start <|> makeFailedParser "Failed at end" 
parseTerm :: Parser Expr
parseTerm = do
    a <- parseFactor 
    go a
    where
        go start = do
            many parseRowSpace 
            mop <- optional $ parseRawToken Plus "+" <|> parseRawToken Minus "-"
            case mop of
                Just op -> do
                    many parseRowSpace
                    b <- parseFactor 
                    case (tokValue op) of
                        "+" -> go $ Add op start b
                        "-" -> go $ Sub op start b
                Nothing -> return start

parseLogAnd :: Parser Expr
parseLogAnd = do
    a <- parseTerm
    go a
    where
        go start = do
            many parseRowSpace 
            mop <- optional $ parseRawToken LogicalAnd "&&"
            case mop of
                Just op -> do
                    many parseRowSpace
                    b <- parseTerm 
                    go $ LogAnd op start b
                Nothing -> return start


parseLogOr :: Parser Expr
parseLogOr = do
    a <- parseLogAnd 
    go a
    where
        go start = do
            many parseRowSpace 
            mop <- optional $ parseRawToken LogicalOr "||"
            case mop of
                Just op -> do
                    many parseRowSpace
                    b <- parseLogAnd 
                    go $ LogOr op start b
                Nothing -> return start

    
parseComp :: Parser Expr
parseComp = do
    a <- parseLogOr 
    go a
    where
        go start = do
            many parseRowSpace 
            mop <- optional $ parseRawToken BiggerEq ">=" <|> parseRawToken Bigger ">" <|> parseRawToken LesserEq "<=" <|> parseRawToken Lesser "<" <|> parseRawToken Equal "==" <|> parseRawToken NotEqual "!="
            case mop of
                Just op -> do
                    many parseRowSpace
                    b <- parseLogAnd
                    case (tokValue op) of
                        ">=" -> go $ CmpBte op start b
                        ">" -> go $ CmpBt op start b
                        "<=" -> go $ CmpLte op start b
                        "<" -> go $ CmpLt op start b
                        "==" -> go $ CmpEq op start b
                        "!=" -> go $ CmpNeq op start b
                Nothing -> return start

-- <range> := <expr> '..' <expr>
parseRange :: Parser Expr
parseRange = do
    a <- parseComp <* many parseRowSpace 
    op <- parseRawToken RngOp ".." <* many parseRowSpace
    b <- parseComp
    return $ Range op a b

parseIn :: Parser Expr
parseIn = do
    id <- parseIdentifierToken <* many parseRowSpace
    op <- parseRawToken InOp "in" <* many parseRowSpace
    rng <- parseRange
    return $ In op id rng 

parseExpr :: Parser Expr
parseExpr = parseClosure <|> parseIf <|> parseIn <|> parseRange <|> parseComp 


parseGroupedExpr :: Parser Expr
parseGroupedExpr = parseChar '(' *> parseExpr <* parseChar ')'

parseExprStatement :: Parser Statement
parseExprStatement = do
    expr <- parseExpr
    many parseRowSpace
    parseChar ';'
    return $ ExprStmt expr

-- <def> := <string> ('->' <string>)* | '(' <def> ')'

parseDef :: Parser PrimeType
parseDef = do
    p <- parseDefWithParen <|> (stringToPrimeType <$> parseKeyword) <* many parseRowSpace
    ps <- many $ do
        parseString "->" <* many parseRowSpace
        parseDefWithParen <|> (stringToPrimeType <$> parseKeyword) <* many parseRowSpace
    return $ CurryType $ p : ps
    where
        stringToPrimeType :: String -> PrimeType
        stringToPrimeType = \case
            "bool" -> BoolType
            "string" -> StringType
            "number" -> NumberType
            s -> CustomType s
        parseDefWithParen :: Parser PrimeType 
        parseDefWithParen = do
            parseChar '(' <* many parseRowSpace
            p <- parseDef <* many parseRowSpace
            parseChar ')' <* many parseRowSpace
            return $ p

parseIndexAssignment :: Parser Statement
parseIndexAssignment = do
    left <- parseListIndex <* many parseRowSpace
    parseChar '=' <* many parseRowSpace
    right <- parseExpr
    return $ Assignment left right

parseIdentAssignment :: Parser Statement
parseIdentAssignment = do
    id <- Literal <$> parseIdentifierToken <* many parseRowSpace
    let (Literal idTok) = id
    params <- many $ parseIdentifierToken <* many parseRowSpace
    let paramMap = foldl' (\b a -> M.insert (tokValue a) (Symbol [] Nothing (M.size b) False) b) M.empty params
    many parseRowSpace
    stack <- gets scopeStack
    case lookupStackTable (tokValue idTok) stack of
        Just _ -> return ()
        Nothing -> do
            top <- gets $ fromJust . S.peekTop . scopeStack 
            modify $ \s -> s {scopeStack = S.adjustStack 0 (M.insert (tokValue idTok) (Symbol params Nothing (M.size top) False)) $ scopeStack s}
    parseChar '=' <* many parseRowSpace
    expr <- parseExpr <* many parseRowSpace
    case expr of
        (Closure addr stmts syms) -> do
            let syms' = M.map (\a -> a {ref = ref a + (length params)}) syms
            return $ Assignment id $ Closure addr stmts $ M.union paramMap syms'
        _ -> if length params == 0 then 
                return $ Assignment id expr 
            else do
                size <- gets $ M.size . fromJust . S.peekTop . scopeStack
                let addr = show size ++ "clsr"
                return $ Assignment id $ Closure addr [ExprStmt expr] paramMap 

parseAssignment :: Parser Statement
parseAssignment = parseIdentAssignment <|> parseIndexAssignment

parseStatement :: Parser Statement
parseStatement = (many parseWhiteSpace) 
    *> ( parseAssignment
    <|> parseExprStatement
    )
    <* (many parseWhiteSpace)

parseClosure :: Parser Expr 
parseClosure = do
    parseChar '{'
    stmts <- gets $ \s -> runParser (many parseStatement) $ s {scopeStack = S.push M.empty $ scopeStack s}
    case stmts of
        Right (a, s') -> do
            let (Just top, rest) = S.pop $ scopeStack s'
            put $ s' {scopeStack = rest}
            parseChar '}'
            let size = M.size top
            let clsrAddr = show size ++ "clsr"
            return $ Closure clsrAddr a top
        Left err -> makeFailedParser $ msg err

parseElif :: Parser Expr 
parseElif = do
    tok <- parseRawToken Keyword "elif" <|> makeFailedParser "Syntax Error: expected keyword 'elif'"
    many parseRowSpace 
    expr <- parseExpr
    many parseWhiteSpace 
    closure <- parseClosure
    return $ Elif tok expr closure 

parseElse :: Parser Expr 
parseElse = do
    tok <- parseRawToken Keyword "else" <|> makeFailedParser "Syntax Error: expected keyword 'else'"
    many parseWhiteSpace
    body <- Else tok <$> parseClosure
    return body

parseIf :: Parser Expr  
parseIf = do
    tok <- parseRawToken Keyword "if" <|> makeFailedParser "Syntax Error: expected keyword 'if'"
    many parseRowSpace 
    expr <- parseExpr <* (many parseWhiteSpace)
    body <- parseClosure <* (many parseWhiteSpace)
    elifs <- many parseElif <* many parseWhiteSpace
    maybeElse <- gets $ runParser parseElse
    case maybeElse of
        Right (a, s) -> do
            put s
            return $ If tok expr body elifs $ Just a
        Left err -> return $ If tok expr body elifs $ Nothing


parseFor :: Parser Expr 
parseFor = do
    tok <- parseRawToken Keyword "For" <|> makeFailedParser "Syntax Error: expected keyword 'for'"
    many parseRowSpace
    cond <- parseIn <* many parseWhiteSpace
    block <- parseClosure
    return $ For tok cond block

parseDefWithId :: Parser (Token, PrimeType) 
parseDefWithId = do
    tok <- parseIdentifierToken <* many parseRowSpace
    parseChar ':' <* many parseRowSpace
    def <- parseDef
    return $ (tok, def)

parseStruct :: Parser ()
parseStruct = do
    stack <- gets $ scopeStack
    if stackSize stack == 1 then do 
        parseRawToken Keyword "struct" <* many parseWhiteSpace
        tok <- parseIdentifierToken <* many parseWhiteSpace
        parseChar '{' <* many parseWhiteSpace
        fields <- many parseDefWithId <* many parseRowSpace <* parseChar ';'
        many parseWhiteSpace
        parseChar '}'
        let sym = StructSym tok $ M.fromList $ map (\t -> (tokValue $ fst t, snd t)) fields
        modify $ \s -> s { scopeStack = insertTop (tokValue tok) sym $ scopeStack s}
        return ()
    else
        makeFailedParser "SyntaxError: strcuts can only be defined at the top-level"


-- f: x: number -> y: number -> z: number {
-- }

parseProgram :: Parser Expr
parseProgram = do 
    prog <- many parseStatement
    syms <- gets $ fromJust . fst . S.pop . scopeStack 
    return $ Closure "0_main" prog syms 
