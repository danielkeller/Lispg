module Main (
    main
) where

import Prelude hiding(catch)
import Syntax
import Semantics
import Evaluator
import General
import Builtins
import Type
import Cps
import CodeGen

import System.Environment
import Control.Exception
import Data.List(intercalate)

import qualified Data.Map as Map

main = do
    args <- getArgs
    case args of
        [] -> evExpr []
        fName:_ -> do
            code <- parseFile fName >>= return.deSugar
            --putStrLn $ printFile code
            flip mapM_ code (\ (Binding v e) ->
                inferT builtinTy e)
            --putStrLn $ intercalate "\n\n" $ map show $ fileToCps code
            --putStrLn $ printCpsFile $ fileToCps code
            --codeGen "test.bc" $ fileToCps code
            evExpr code

evExpr code = do
    catch doRep $ \e -> do
        let (InterpreterException str) = e :: InterpreterException
        putStrLn str
    evExpr code
    where doRep = do
              text <- getLine
              let replCode = dsExpr $ parseInput text
              putStrLn $ printExpr replCode
              inferT builtinTy replCode
              --putStrLn $ printCps $ toCps replCode 
              --print $ doInfer code replCode
              --putStrLn $ printValue $ doEval code replCode

doEval file expr = eval (ELetRec file expr) builtins
doInfer file expr = infer builtinTy (ELetRec file expr)
