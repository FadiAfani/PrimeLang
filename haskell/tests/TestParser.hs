module Main where

import Control.Monad.State.Lazy
import Parser
import Test.HUnit
import Utils
import Token
import ScopeStack
import Stack

testParseList = TestCase ( let 
    st = ParserState (1,0) "[1,1,1]" initStack
    lit1 = Literal $ Token (1,2) "1" Number
    lit2 = Literal $ Token (1,4) "1" Number
    lit3 = Literal $ Token (1,6) "1" Number
    in case evalStateT parseList st of
        Right a -> assertEqual "test successful parsing of a list with numerical elements" (List Nothing [lit1, lit2, lit3]) a
        Left err -> assertFailure "parser failed to produce any meaningful results"
    )

testParseListIndex = TestCase ( let
    st = ParserState (1,0) "arr[0]" initStack
    idx = Token (1,3) "arr" Identifier 
    lit = Literal $ Token (1,5) "0" Number
    in case evalStateT parseListIndex st of
        Right a -> assertEqual "test successful parsing of a list indexing operation" (ListIndex idx lit) a
        Left err -> assertFailure "failure in parsing an indexing operation"
    )

tests = TestList [TestLabel "testParseList" testParseList, TestLabel "testParseListIndex" testParseListIndex]

main :: IO Counts 
main = runTestTT tests 
