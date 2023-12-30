module Main where
import Data.Maybe (fromJust)
import Control.Monad.State.Lazy
import Test.HUnit
import Parser
import Token
import ScopeStack 
import Typechecker
import Stack
import qualified Data.Map as M
import Types 
import Utils 

-- Tests for the correct identification of 'Outer Values'
-- An outer value is a variable that is closed-over
-- meaning it is used in an inner scope where it is not originally defined

testOuterValue1 = TestCase (let  
    var = mkDummyToken "x" Identifier
    clsr = Closure "" [ExprStmt $ Literal $ mkDummyToken "x" Identifier] $ M.fromList []
    mainClsr = Closure "" [Assignment var (Literal $ Token (0,0) "5" Number), ExprStmt clsr] $ M.fromList [("x", mkDummySymbol)]
    in case evalStateT (updateExpr mainClsr) (push (blockSyms mainClsr) $ initStack) of
        Right a -> let
            var' = fromJust $ M.lookup "x" $ blockSyms a
            in assertEqual "test successful outer value detection" True $ isOuterValue var'
        Left err -> assertFailure "updateExpr failed to produce any meaningful results"
    )

-- x = number + number -> x: number 
testAddNumber = TestCase ( let
    var = mkDummyToken "x" Identifier
    num = mkDummyToken "1" Number
    tok = mkDummyToken "+" Plus
    stmt = Assignment var $ Add tok (Literal num) (Literal num)
    in case execStateT (updateStatement stmt) (push (M.fromList [("x", mkDummySymbol)]) $ initStack) of
        Right s -> let 
            varType = fromJust $ lookupStackTable "x" s
            in assertEqual "test successful type inference of binary number addition assigned to a variable" (Just NumberType) $ symType varType
        Left err -> assertFailure "updateStatement failed to produce any meaningful results"
    )

-- x = number * number -> x: number 
testMultNumber = TestCase ( let
    var = mkDummyToken "x" Identifier
    num = mkDummyToken "1" Number
    tok = mkDummyToken "*" Plus
    stmt = Assignment var $ Mult tok (Literal num) (Literal num)
    in case execStateT (updateStatement stmt) (push (M.fromList [("x", mkDummySymbol)]) $ initStack) of
        Right s -> let 
            varType = fromJust $ lookupStackTable "x" s
            in assertEqual "test successful type inference of binary number addition assigned to a variable" (Just NumberType) $ symType varType
        Left err -> assertFailure "updateStatement failed to produce any meaningful results"
    )
-- x = number / number -> x: number
-- also check for the case where the denominator is 0

testDivNumber = TestCase ( let
    var = mkDummyToken "x" Identifier
    num = mkDummyToken "1" Number
    tok = mkDummyToken "/" Plus
    stmt = Assignment var $ Div tok (Literal num) (Literal num)
    in case execStateT (updateStatement stmt) (push (M.fromList [("x", mkDummySymbol)]) $ initStack) of
        Right s -> let 
            varType = fromJust $ lookupStackTable "x" s
            in assertEqual "test successful type inference of binary number addition assigned to a variable" (Just NumberType) $ symType varType
        Left err -> assertFailure "updateStatement failed to produce any meaningful results"
    )

testDivByZero = TestCase ( let
    num = mkDummyToken "1" Number
    zero = mkDummyToken "0" Number
    tok = mkDummyToken "/" Plus
    expr = Div tok (Literal num) (Literal zero)
    in case execStateT (updateExpr expr) (push (M.fromList []) $ initStack) of
        Right s -> assertFailure "division by zero should not compile" 
        Left err -> assertEqual "" True True 
    )

testListType = TestCase ( let
    lit = Literal $ mkDummyToken "1" Number
    list = List Nothing [lit, lit]
    in case evalStateT (getExprType list) (push (M.fromList [] ) $ initStack ) of
        Right a -> assertEqual "test successful type inference of a simple list of numbers" (Just NumberType) a
        Left err -> assertFailure "failed to infer the list type"
    )


testInavlidListType = TestCase ( let
    lit = Literal $ mkDummyToken "1" Number
    lit' = Literal $ mkDummyToken "str" PString
    list = List Nothing [lit, lit']
    in case evalStateT (getExprType list) (push (M.fromList [] ) $ initStack ) of
        Right a -> assertFailure "compiler error expected" 
        Left err -> assertBool "" True 
    )


tests = TestList [
    TestLabel "testSimpleOuterVal" testOuterValue1, 
    TestLabel "testAddNumber" testAddNumber, 
    TestLabel "testMultNumber" testMultNumber,
    TestLabel "testDivNumber" testDivNumber,
    TestLabel "testDivByZero" testDivByZero,
    TestLabel "testListType" testListType,
    TestLabel "testInavlidListType" testInavlidListType
    ]

main :: IO Counts 
main = runTestTT tests
