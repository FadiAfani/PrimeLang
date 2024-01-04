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

testParseStruct = TestCase ( let
    st = ParserState (1,0) "struct test { field1: Number; \n field2: String }" initStack
    in case execStateT parseStruct st of
        Right a -> assertEqual "test struct parsing (tests if the string was consumed)" "" $ input a
        Left err -> assertFailure "failure in parsing a struct"
        )

testParseStructInit = TestCase ( let 
    st = ParserState (1,0) "test { 10, 20 }" initStack
    in case execStateT parseInitStruct st of
        Right a -> assertEqual "test struct initialization" "" $ input a 
        Left err -> assertFailure "failure in parsing a struct initialization block"
        )
testParseStructFieldAccess = TestCase ( let 
    st = ParserState (1,0) "structName.field1" initStack
    in case execStateT parseStructFieldAccess st of
        Right a -> assertEqual "test struct field access" "" $ input a
        Left err -> assertFailure "failure in parsing a struct field access"
        )

testParseStructInitWithKey = TestCase ( let
    st = ParserState (1,0) "structName { field1: 10, field2: 20 }" initStack
    in case execStateT parseInitStruct st of
        Right a -> assertEqual "test struct initialization using key-value syntax" "" $ input a
        Left err -> assertFailure "failure in parsing a key-value struct field"
    )

tests = TestList [TestLabel "testParseList" testParseList,
    TestLabel "testParseListIndex" testParseListIndex,
    TestLabel "testParseStruct" testParseStruct,
    TestLabel "testParseStructInit" testParseStructInit,
    TestLabel "testParseStructFieldAccess" testParseStructFieldAccess,
    TestLabel "testParseStructInitWithKey" testParseStructInitWithKey
    ]

main :: IO Counts 
main = runTestTT tests 
