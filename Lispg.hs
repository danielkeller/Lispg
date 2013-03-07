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

import System.Environment
import Control.Exception

main = do
    fName <- getArgs >>= return.head
    code <- parseFile fName >>= return.deSugar
    putStrLn $ printFile code
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
              putStrLn $ printType $ doInfer replCode
              putStrLn $ printValue $ doEval code $ replCode

doEval file expr = eval (ELetRec file expr) builtins
doInfer code = infer builtinTy code
