{-# LANGUAGE FlexibleInstances #-}

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

class TyPart a where
    toTy :: a -> Type
instance TyPart [Char] where
    toTy s = TV (TVar s)
instance TyPart Constr where
    toTy c = Type c []
instance TyPart Type where
    toTy = id

(==>) :: (TyPart a, TyPart b) => a -> b -> Type
(==>) l r = Type Func [toTy l, toTy r]
infixr 5 ==>
list_of :: TyPart a => a -> Type
list_of t = Type List [toTy t]
for_all :: [String] -> Type -> Scheme
for_all ss t = Scheme (map TVar ss) t

builtinTy = Map.fromList [
     ("cons", for_all ["a"] $ "a" ==> list_of "a" ==> list_of "a")
    ,("car", for_all ["a"] $ list_of "a" ==> "a")
    ,("cdr", for_all ["a"] $ list_of "a" ==> list_of "a")
    ,("eq?", for_all ["a"] $ "a" ==> "a" ==> AtomTy)
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
    ,("not", tyTest)
    ,("zero?", for_all [] $ NumberTy ==> AtomTy)
    ,("null?", tyTest)
    ,("list?", tyTest)
    ,("or", for_all [] $ AtomTy ==> AtomTy ==> AtomTy)
    ,("and", for_all [] $ AtomTy ==> AtomTy ==> AtomTy)
    ]
    where math = for_all [] $ NumberTy ==> NumberTy ==> NumberTy
          test = for_all [] $ NumberTy ==> NumberTy ==> AtomTy
          tyTest = for_all ["a"] $ "a" ==> AtomTy
