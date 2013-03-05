module Evaluator (
    eval,
    doEval,
    printValue
) where

import Semantics
import Syntax
import General
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
    ,("=", derived "eq?") --because this works on numbers too
    ,("not", derived "(lambda (b) (if b #f #t))")
    ,("zero?", derived "(lambda (n) (= n 0))")
    ,("null?", derived "(lambda (v) (eq? v '()))")
    ,("list?", derived "(lambda (v) (or (pair? v) (eq? v '())))")
    ,("or", derived "(lambda (l r) (if (not l) r l))")
    ,("and", derived "(lambda (l r) (if l r l))")
    ]
    where toBin f = Builtin $ \ a -> Builtin $ \ b -> f a b
          binMath f = toBin $ \ (Number l) (Number r) -> Number (f l r)
          derived code = eval (dsExpr $ parseInput code) builtins
          isAtom (Atom _) = valTrue
          isAtom _ = valFalse
          isNumber (Number _) = valTrue
          isNumber _ = valFalse
          isPair (_ :. _) = valTrue
          isPair _ = valFalse
          isEq l r = if l == r then valTrue else valFalse

doEval file expr = eval (ELetRec file expr) builtins

eval :: Expr -> Env -> Value
eval (ELit l) _ = l
eval (EVar s) env = case Map.lookup s env of
    Nothing -> throwEx $ s ++ " is undefined"
    Just v -> v
eval l@(EAbs _ _) env = Closure l env

eval (EApp fun arg) env =
    doFun (eval fun env) (eval arg env)
    where doFun (Closure (EAbs p b) cenv) arg = 
              eval b (Map.insert p arg cenv)
          doFun (Builtin f) arg = f arg
          doFun bad _ = throwEx $ "call on non-function " ++ printValue bad

eval (ELet bs expr) env =
    eval expr $ Map.union env $ Map.fromList $ zip (map theVar bs) (map theVal bs)
    where theVar (Binding v _) = v
          theVal (Binding _ e) = eval e env

eval (ELetRec bs expr) env = eval expr newEnv
    where newEnv = Map.union env $ Map.fromList $ zip (map theVar bs) (map theVal bs)
          theVar (Binding v _) = v
          theVal (Binding _ e) = eval e newEnv
          --these definitions are mutually recursive, which is a (sort of) cheating way
          --to get letrec to work

eval (ECase (Alt c v : alts)) env = doCase $ eval c env
    where doCase b
              | b /= Atom "#f" = eval v env
              | otherwise = eval (ECase alts) env 
