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
     ("cons", Scheme ["a"] $ TFun (TVar "a") (TFun (TList (TVar "a")) (TList (TVar "a"))))
    ,("car", Scheme ["a"] $ TFun (TList (TVar "a")) (TVar "a"))
    ,("cdr", Scheme ["a"] $ TFun (TList (TVar "a")) (TList (TVar "a")))
    ,("eq?", Scheme ["a"] $ TFun (TVar "a") (TFun (TVar "a") (Ty "Atom")))
    ,("atom?", tyTest)
    ,("number?", tyTest)
    ,("pair?", tyTest)
    ,("+", math)
    ,("-", math)
    ,("*", math)
    ,("quotient", math)
    ,("remainder", math)
    ,("<", test)
    ,(">", test)
    ,("<=", test)
    ,(">=", test)
    ,("=", test)
    ,("=", test) --because this works on numbers too
    ,("not", Scheme["a"] $ TFun (TVar "a") (Ty "Atom"))
    ,("zero?", Scheme[] $ TFun (Ty "Number") (Ty "Atom"))
    ,("null?", tyTest)
    ,("list?", tyTest)
    ,("or", Scheme [] $ TFun (Ty "Atom") (TFun (Ty "Atom") (Ty "Atom")))
    ,("and", Scheme [] $ TFun (Ty "Atom") (TFun (Ty "Atom") (Ty "Atom")))
    ]
    where math = Scheme [] $ TFun (Ty "Number") (TFun (Ty "Number") (Ty "Number"))
          test = Scheme [] $ TFun (Ty "Number") (TFun (Ty "Number") (Ty "Atom"))
          tyTest = Scheme["a"] $ TFun (TVar "a") (Ty "Atom")
