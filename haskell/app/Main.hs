module Main where

import System.Environment
import Compiler
import Typechecker
import Parser
import ScopeStack
import qualified Data.ByteString as B
import qualified Stack as S
import qualified Data.Map as M
import Data.List (sortOn)
import Control.Monad.State.Lazy

main :: IO ()
main = do
    [inp, out] <- getArgs
    inp' <- readFile inp
    case runParser parseProgram (ParserState {input=inp', curPos=(1,1), scopeStack = S.push M.empty $ S.initStack}) of
        Right (a, s) -> do
            case evalStateT (updateExpr a) (scopeStack s) of
                Right e -> case runState (compileExpr e) (M.empty, S.initStack) of
                    (bytes, compState) -> do
                        print e
                        let consts = concatMap (fst . snd) $ sortOn (snd . snd) $ M.toList $ fst compState
                        print bytes
                        print $ consts 
                        B.writeFile out $ B.pack consts
                Left err -> print err
        Left err -> print err
