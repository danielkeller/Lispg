module Main (
    main
) where

import Syntax
import Semantics
import Evaluator

import System.Environment

main = do
    fName <- getArgs >>= return.head
    code <- parseFile fName >>= return.deSugar
    putStrLn $ printFile code
    interact (unlines . map (evExpr code) . lines)

evExpr code ex = printExpr replCode ++ "\n" ++ (printValue $ doEval code $ replCode)
    where replCode = dsExpr $ parseInput ex
