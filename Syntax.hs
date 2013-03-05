module Syntax (
    parseFile,
    parseInput,
    printValue,
) where

import General
import Text.ParserCombinators.Parsec
import Control.Exception

printValue l@(_:._) = "(" ++ plHelp l ++ ")"
printValue (Closure _ _) = "#<closure>"
printValue (Builtin _) = "#<closure>"
printValue l = plHelp l
plHelp Nil = "()"
plHelp (Atom s) = s
plHelp (Number n) = show n
plHelp (l@(_:._) :. r) = "(" ++ plHelp l ++ ") " ++ plHelp r
plHelp (l :. Nil) = plHelp l
plHelp (l :. r) = plHelp l ++ " " ++ plHelp r

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseExpr = parseSexp <|> parseNumber <|> parseAtom <|> parseQuote

parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom $ first:rest

parseNumber :: Parser Value
parseNumber = do
    text <- many1 digit
    return $ Number $ read text

parseQuote = do
    char '\''
    conts <- parseExpr
    return $ Atom "quote" :. conts :. Nil

parseSexp = do
    char '('
    parseSexpRest

parseSexpBody = do
    first <- parseExpr
    rest <- parseSexpRest 
    return $ first :. rest

parseSexpRest = do
    spaces
    parseSexpBody <|> (char ')' >> return Nil)

parseSexps = do
    spaces
    first <- parseSexp
    spaces
    rest <- parseSexps <|> (eof >> return Nil)
    return $ first :. rest

parseFile fName = do
    input <- readFile fName
    return $ case parse parseSexps fName input of
        Left err -> error (show err)
        Right val -> val

parseInput input =
    case parse parseExpr "input" input of
        Left err -> throwEx (show err)
        Right val -> val
