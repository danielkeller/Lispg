{-# LANGUAGE FlexibleInstances, DoRec #-}

module Type (
    infer,
    Type(..),
    printType,
) where

import Data.List(intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import General

data Type = TVar String
          | Ty String --concrete type
          | TFun Type Type
          | TList Type
          | Top
          | Scheme [String] Type --universal quantification
          deriving (Eq, Show)

printType (TVar s) = s
printType (Ty s) = s
printType (TFun a@(TFun _ _) r) = "(" ++ printType a ++ ") -> " ++ printType r
printType (TFun a r) = printType a ++ " -> " ++ printType r
printType (TList t) = "[" ++ printType t ++ "]"
printType Top = "⊤"
printType (Scheme free t) = "∀ " ++ intercalate "," free ++ " . " ++ printType t

type Subst = Map.Map String Type
nullSubst :: Subst
nullSubst = Map.empty
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = (Map.map (apply s1) s2) `Map.union` s1

class Types a where
    ftv :: a -> Set.Set String
    apply :: Subst -> a -> a

instance Types Type where
    ftv (TVar s) = Set.singleton s
    ftv (TFun a r) = ftv a `Set.union` ftv r
    ftv (TList t) = ftv t
    ftv (Scheme v t) = ftv t `Set.difference` Set.fromList v
    ftv _ = Set.empty

    apply s (TVar n) = case Map.lookup n s of
            Nothing -> TVar n
            Just t -> t
    apply s (TFun a r) = TFun (apply s a) (apply s r)
    apply s (TList t) = TList (apply s t)
    apply s (Scheme v t) = Scheme v (apply (foldr Map.delete s v) t)
    apply _ t = t

instance Types [Type] where
    ftv l = foldr Set.union Set.empty (map ftv l)
    apply s = map (apply s)

type TypeEnv = Map.Map String Type

remove :: TypeEnv -> String -> TypeEnv
remove env v = Map.delete v env

instance Types TypeEnv where
    ftv env = ftv (Map.elems env)
    apply s env = Map.map (apply s) env

generalize :: TypeEnv -> Type -> Type
generalize env t = Scheme vars t
    where vars = Set.toList $ ftv t `Set.difference` ftv env

type TI a = State Int a

evalTI :: TI a -> a
evalTI t = evalState t 0

newTyVar :: String -> TI Type
newTyVar prefix = do
    cur <- get
    put (cur + 1)
    return $ TVar $ prefix ++ show cur

instantiate :: Type -> TI Type
instantiate (Scheme v t) = do
    nvars <- mapM (\_ -> newTyVar "a") v
    let s = Map.fromList (zip v nvars)
    return $ apply s t
instantiate other = return other
--instantiate bad = error $ "can't instantiate " ++ printType bad

mgu :: Type -> Type -> (Subst, Type)
mgu (TFun a r) (TFun a' r') = let
    (s1, a'') = mgu a a'
    (s2, r'') = mgu (apply s1 r) (apply s1 r')
    in ((s1 `composeSubst` s2), TFun a'' r'')
mgu (TList t) (TList t') = let
    (s, t'') = mgu t t'
    in (s, TList t'')
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu t u = if t == u
    then (nullSubst, t)
    else (nullSubst, Top) 

varBind :: String -> Type -> (Subst, Type)
varBind u t
    | t == TVar u = (nullSubst, t)
    | u `Set.member` ftv t = (Map.singleton u Top, Top)
    | otherwise = (Map.singleton u t, t)

tiLit :: Value -> TI Type
tiLit (Atom _) = return $ Ty "Atom"
tiLit (Number _) = return $ Ty "Number"
tiLit Nil = do
    conts <- newTyVar "l" --i think
    return $ TList conts
tiLit (v :. Nil) = do
    lTy <- tiLit v
    return $ TList lTy
tiLit (v :. rest) = do
    lTy <- tiLit v 
    restTy <- tiLit rest
    return $ theTy restTy lTy
    where theTy (TList t) lTy
              | t == lTy = TList t
              | otherwise = Top
          theTy _ _ = Top
tiLit _ = error "unexpected run-time value in literal"

ti :: TypeEnv -> Expr -> TI (Subst, Type)
ti _ (ELit l) = do
    litTy <- tiLit l
    return (nullSubst, litTy)

ti env (EVar n) = --do special types here
    case Map.lookup n env of
        Nothing -> throwEx $ "unbound variable " ++ n
        Just sigma -> do
            t <- instantiate sigma
            return (nullSubst, t)

ti env (EAbs n e) = do
    tv <- newTyVar "p"
    let env' = remove env n --hiding
        env'' = env' `Map.union` (Map.singleton n (Scheme [] tv))
    (s1, t1) <- ti env'' e
    return (s1, TFun (apply s1 tv) t1)

ti env exp@(EApp f a) = do
    tv <- newTyVar "a"
    (s1, t1) <- ti env f
    (s2, t2) <- ti (apply s1 env) a
    let (s3, t3) = mgu (apply s1 t1) (TFun t2 tv)
    return (s3 `composeSubst` s2 `composeSubst` s1, case t3 of
            TFun _ a' -> a'
            Top -> Top)

ti env (ELet bs e2) = do
    let env' = foldl remove env $ map var bs --hiding
    st1s <- mapM (doBind env') bs
    let env'' = Map.union env' $ Map.fromList $ zip (map var bs) (map snd st1s)
        alls = foldl1 composeSubst $ map fst st1s
    (s2, t2) <- ti (apply alls env'') e2
    return (s2 `composeSubst` alls, t2)
    where doBind env (Binding x e1) = do
              (s1, t1) <- ti env e1
              return (s1, generalize (apply s1 env) t1) --let-polym
          var (Binding x _) = x

ti env (ELetRec bs e2) = do
    nvars <- mapM (\_ -> newTyVar "lr") bs --needed to terminate in some cases
    let env' = Map.fromList (zip (map var bs) nvars) `Map.union` env
    subs <- mapM (doBind env') bs
    let alls = foldl unifySubst nullSubst subs
    (s2, t2) <- ti (apply alls env') e2
    return (s2 `composeSubst` alls, t2)
    where doBind env (Binding x e1) = do
              (s1, t1) <- ti env e1
              let TVar myV = env Map.! x
              let s2 = s1 `composeSubst` Map.singleton myV t1
              return s2
          unifySubst s1 s2 
              | int == Map.empty = s1 `composeSubst` s2
              | otherwise = Map.foldl unifySubst nullSubst int `composeSubst` s1 `composeSubst` s2
              where int = Map.intersectionWith mgus s1 s2
          mgus s1 s2 = fst (mgu s1 s2)
          var (Binding x _) = x

ti env (ECase alts) = do
    tv <- newTyVar "c"
    doAlts alts nullSubst tv
    where doAlts ((Alt _ e):alts) s t = do
              (s1, t1) <- ti (apply s env) e
              let (s2, t2) = mgu (apply s1 t) t1
              doAlts alts (s2 `composeSubst` s1) t2
          doAlts [] s t = return (s, t)

infer :: TypeEnv -> Expr -> Type
infer env e = evalTI $ do
    (s, t) <- ti env e
    return (apply s t)
