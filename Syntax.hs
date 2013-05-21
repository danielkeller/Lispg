module Syntax (
    parseFile,
    parseInput,
) where

import General
import Text.Parsec
import Control.Exception hiding (try)

symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

wspace = skipMany (space <|> comment <?> "")
    where comment = do
              char ';'
              manyTill anyChar newline
              return ' '

parseExpr = parseSexp <|> parseNumber <|> parseAtom <|> parseQuote <?> "expression"

parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom $ first:rest

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
    wspace
    parseSexpBody <|> do
        char ')' <?> ")"
        return Nil

parseSexps = do
    wspace
    first <- parseSexp
    wspace
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
