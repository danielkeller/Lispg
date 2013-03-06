{-# LANGUAGE FlexibleInstances #-}

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
          deriving (Eq)

printType (TVar s) = s
printType (Ty s) = s
printType (TFun a@(TFun _ _) r) = "(" ++ printType a ++ ") -> " ++ printType r
printType (TFun a r) = printType a ++ " -> " ++ printType r
printType (TList t) = "[" ++ printType t ++ "]"
printType Top = "⊤"
printType (Scheme free t) = "∀" ++ intercalate "," free ++ printType t

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
    ftv l = foldr1 Set.union (map ftv l)
    apply s = map (apply s)

newtype TypeEnv = TypeEnv (Map.Map String Type)

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) v = TypeEnv (Map.delete v env)

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (Map.elems env)
    apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

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

mgu :: Type -> Type -> TI (Subst, Type)
mgu (TFun a r) (TFun a' r') = do
    (s1, a'') <- mgu a a'
    (s2, r'') <- mgu (apply s1 r) (apply s1 r')
    return ((s1 `composeSubst` s2), TFun a'' r'')
mgu (TList t) (TList t') = do
    (s, t'') <- mgu t t'
    return (s, TList t'')
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu t u = return $ if t == u
    then (nullSubst, t)
    else (nullSubst, Top) 

varBind :: String -> Type -> TI (Subst, Type)
varBind u t
    | t == TVar u = return (nullSubst, t)
    | u `Set.member` ftv t = return (Map.singleton u Top, Top)
    | otherwise = return (Map.singleton u t, t)

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

ti (TypeEnv env) (EVar n) =
    case Map.lookup n env of
        Nothing -> throwEx $ "unbound variable " ++ n
        Just sigma -> do
            t <- instantiate sigma
            return (nullSubst, t)

ti env (EAbs n e) = do
    tv <- newTyVar "p"
    let TypeEnv env' = remove env n --hiding
        env'' = TypeEnv (env' `Map.union` (Map.singleton n (Scheme [] tv)))
    (s1, t1) <- ti env'' e
    return (s1, TFun (apply s1 tv) t1)

ti env exp@(EApp f a) = do
    tv <- newTyVar "a"
    (s1, t1) <- ti env f
    (s2, t2) <- ti (apply s1 env) a
    (s3, t3) <- mgu (apply s1 t1) (TFun t2 tv)
    return (s3 `composeSubst` s2 `composeSubst` s1, t3)

ti env (ELet bs e2) = do
    let TypeEnv env' = foldl remove env $ map var bs --hiding
    st1s <- mapM doBind bs
    let env'' = TypeEnv $ Map.union env' $ Map.fromList $ zip (map var bs) (map snd st1s)
        alls = foldl1 composeSubst $ map fst st1s
    (s2, t2) <- ti (apply alls env'') e2
    return (alls, t2)
    where doBind (Binding x e1) = do
              (s1, t1) <- ti env e1
              return (s1, generalize (apply s1 env) t1) --let-polym
          var (Binding x _) = x

ti _ (ELetRec _ _) = error "TODO"
ti _ (ECase _) = error "TODO"

--infer :: Map.Map String Type -> Expr -> Type
infer :: Expr -> Type
infer e = evalTI $ do
    (s, t) <- ti (TypeEnv Map.empty) e
    return (apply s t)
