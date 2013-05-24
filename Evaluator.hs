module Evaluator (
    eval,
) where

import Semantics
import Syntax
import General
import qualified Data.Map as Map

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
          doFun bad _ = throwEx $ "call on non-function " ++ show bad

eval (ELet bs expr) env = eval expr $ Map.union env $ Map.fromList $ map bPair bs
    where bPair (Binding v e) = (v, eval e env)

eval (ELetRec bs expr) env = eval expr newEnv
    where newEnv = Map.union env $ Map.fromList $ map bPair bs
          bPair (Binding v e) = (v, eval e newEnv)
          --these definitions are mutually recursive, which is a (sort of) cheating way
          --to get letrec to work

eval (ECase (Alt c v : alts)) env = doCase $ eval c env
    where doCase b
              | b /= Atom "#f" = eval v env
              | otherwise = eval (ECase alts) env 
