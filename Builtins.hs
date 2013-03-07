module Builtins (
    builtins,
    builtinTy
) where

import Semantics
import Syntax
import General
import Evaluator
import Type
import qualified Data.Map as Map

valTrue = Atom "#t"
valFalse = Atom "#f"
valUndef = Atom "#<undefined>"

builtins = Map.fromList [
     ("cons", toBin (:.))
    ,("car", Builtin (\ (a :. _) -> a))
    ,("cdr", Builtin (\ (_ :. d) -> d))
    ,("eq?", toBin isEq)
    ,("atom?", Builtin isAtom)
    ,("number?", Builtin isNumber)
    ,("pair?", Builtin isPair)
    ,("+", binMath (+))
    ,("-", binMath (-))
    ,("*", binMath (*))
    ,("quotient", binMath quot)
    ,("remainder", binMath rem)
    ,("<", comp (<))
    ,(">", comp (>))
    ,("<=", comp (<=))
    ,(">=", comp (>=))
    ,("=", derived "eq?") --because this works on numbers too
    ,("not", derived "(lambda (b) (if b #f #t))")
    ,("zero?", derived "(lambda (n) (= n 0))")
    ,("null?", derived "(lambda (v) (eq? v '()))")
    ,("list?", derived "(lambda (v) (or (pair? v) (eq? v '())))")
    ,("or", derived "(lambda (l r) (if l l r))")
    ,("and", derived "(lambda (l r) (if l r l))")
    ]
    where toBin f = Builtin $ \ a -> Builtin $ \ b -> f a b
          toBool v = if v then valTrue else valFalse
          binMath f = toBin $ \ (Number l) (Number r) -> Number (f l r)
          comp f = toBin $ \ (Number l) (Number r) -> toBool (f l r)
          derived code = eval (dsExpr $ parseInput code) builtins
          isAtom (Atom _) = valTrue
          isAtom _ = valFalse
          isNumber (Number _) = valTrue
          isNumber _ = valFalse
          isPair (_ :. _) = valTrue
          isPair _ = valFalse
          isEq l r = toBool (l == r)

builtinTy = Map.fromList [
    ("cons", TFun (TVar "a") (TFun (TList (TVar "a")) (TList (TVar "a")))),
    ("car", TFun (TList (TVar "a")) (TVar "a"))
    ]
