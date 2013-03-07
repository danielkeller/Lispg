{-# LANGUAGE DeriveDataTypeable #-}

module General (
    Env(..),
    Value(..),
    File,
    Binding(..),
    Expr(..),
    Alt(..),
    InterpreterException(..),
    throwEx,
    printValue,
    printExpr,
    printFile,
) where

import qualified Data.Map as Map
import Control.Exception
import Data.Typeable
import Data.List(intercalate)

data InterpreterException = InterpreterException String
    deriving (Show, Typeable)
instance Exception InterpreterException
throwEx = throw . InterpreterException

type Env = Map.Map String Value

data Value = Atom String | Number Integer | Value :. Value | Nil --compile-time
             | Closure Expr Env | Builtin (Value -> Value) --run-time

infixr 5 :.

type File = [Binding]
data Binding = Binding String Expr
data Expr = ELit Value
          | EVar String
          | EAbs String Expr
          | EApp Expr Expr
          | ELet [Binding] Expr
          | ELetRec [Binding] Expr
          | ECase [Alt]
data Alt = Alt Expr Expr

--this operation is only defined on atoms and numbers
instance Eq Value where
    (Atom l) == (Atom r) = l == r
    (Number l) == (Number r) = l == r
    Nil == Nil = True
    _ == _ = False

printValue l@(_:._) = "(" ++ plHelp l ++ ")"
printValue (Closure _ _) = "#<closure>"
printValue (Builtin _) = "#<closure>"
printValue l = plHelp l
plHelp Nil = "()"
plHelp (Atom s) = s
plHelp (Number n) = show n
plHelp (l@(_:._) :. Nil) = "(" ++ plHelp l ++ ")"
plHelp (l@(_:._) :. r@(_ :. _)) = "(" ++ plHelp l ++ ") " ++ plHelp r
plHelp (l@(_:._) :. r) = "(" ++ plHelp l ++ ") . " ++ plHelp r
plHelp (l :. Nil) = plHelp l
plHelp (l :. r@(_ :. _)) = plHelp l ++ " " ++ plHelp r
plHelp (l :. r) = plHelp l ++ " . " ++ plHelp r

printExpr (ELit l) = '\'' : printValue l
printExpr (EVar s) = s
printExpr (EAbs a e) = "λ" ++ a ++ "." ++ printExpr e
printExpr (ELet b e) = "let " ++ intercalate "\n    " (printBindings b) ++ "\n    in " ++ printExpr e
printExpr (ELetRec b e) = printExpr $ ELet b e
printExpr (EApp l r@(EApp _ _)) = printExpr l ++ " (" ++ printExpr r ++ ")"
printExpr (EApp l r@(EAbs _ _)) = printExpr l ++ " (" ++ printExpr r ++ ")"
printExpr (EApp l r) = printExpr l ++ " " ++ printExpr r
printExpr (ECase alts) = "case\n" ++ intercalate "\n" (map printCase alts)
    where printCase (Alt c e) = "    | " ++ printExpr c ++ " -> " ++ printExpr e

printBindings (Binding v e : bs) = (v ++ " = " ++ printExpr e) : printBindings bs
printBindings [] = []

printFile f = intercalate "\n\n" (printBindings f) ++ "\n\n"
