{-# LANGUAGE FlexibleInstances #-}

module General (
    Env(..),
    Value(..),
    File,
    Binding(..),
    Expr(..),
    Alt(..)
) where

import Data.Map as Map

type Env = Map String Value

data Value = Atom String | Number Integer | Value :. Value | Nil --compile-time
             | Closure Expr Env | Builtin (Value -> Value) --run-time

infixr 5 :.

--this operation is only defined on atoms and numbers
instance Eq Value where
    (Atom l) == (Atom r) = l == r
    (Number l) == (Number r) = l == r
    Nil == Nil = True
    _ == _ = False

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
