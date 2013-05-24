{-# LANGUAGE FlexibleInstances #-}

module Type (
    infer,
    inferT,
    Type(..),
    Constr(..),
    TVar(..),
    Scheme(..),
) where

import Data.List(intercalate)
import Data.Maybe
import Text.Show
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import General

import Debug.Trace

lookupOr :: Ord k => a -> Map.Map k a -> k -> a
lookupOr d m k = fromMaybe d $ Map.lookup k m

data Constr = AtomTy | NumberTy | Func | List | Top
    deriving (Eq, Show)

newtype TVar = TVar String
    deriving (Eq, Ord)

data Type = TExp TVar Constr [Type]
          | Type Constr [Type]
          | TV TVar
          deriving Eq

data Scheme = Scheme [TVar] Type --universal quantification
    deriving Eq

instance Show TVar where
    showsPrec _ (TVar s) = showString s

instance Show Type where
    showsPrec d (TV v) = showsPrec d v
    showsPrec d (TExp v c ts) = showsPrec d v . showString " = " . showsPrec 6 (Type c ts)
    showsPrec d (Type Func [a,r]) = showParen (d>5) $
        showsPrec 6 a . showString " -> " . showsPrec 5 r
    showsPrec _ (Type List [c]) = showString "[" . showsPrec 0 c . showString "]"
    showsPrec _ (Type Top _) = showString "⊤"
    showsPrec _ (Type a _) = showsPrec 0 a

instance Show Scheme where
    showsPrec _ (Scheme [] t) = showsPrec 0 t
    showsPrec _ (Scheme free t) = showString ("∀ " ++ intercalate "," (map show free) ++ " . ") . showsPrec 0 t

newtype Subst = Subst (Map.Map TVar Type)

instance Show Subst where
    showsPrec _ (Subst s) = showListWith printS (Map.toAscList s)
        where printS (var, t) = showsPrec 0 var . showString " => " . showsPrec 0 t

nullSubst :: Subst
nullSubst = Subst Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst (Subst l) (Subst r) = Subst $ Map.map (apply $ Subst l) r `Map.union` l

composeSubsts :: [Subst] -> Subst
composeSubsts = foldl composeSubst nullSubst

topSubst :: TVar -> Subst
topSubst v = Subst (Map.singleton v (Type Top []))

class Types a where
    ftv :: a -> Set.Set TVar
    apply :: Subst -> a -> a

instance Types Type where
    ftv (TV v) = Set.singleton v
    ftv (Type _ ts) = Set.unions $ map ftv ts
    ftv (TExp v _ ts) = Set.insert v $ Set.unions $ map ftv ts --i think
    apply (Subst s) (TV v) = case Map.lookup v s of
                                 Just t -> t
                                 Nothing -> TV v
    apply (Subst s) t@(TExp v c ts) = case Map.lookup v s of
                                          Just u -> u
                                          Nothing -> TExp v c $ apply (Subst s) ts
    apply s (Type c ts) = Type c $ map (apply s) ts

instance Types Scheme where
    ftv (Scheme v t) = ftv t `Set.difference` Set.fromList v
    apply (Subst s) (Scheme v t) = Scheme v $ apply (Subst $ foldr Map.delete s v) t

instance Types a => Types [a] where
    ftv l = foldr Set.union Set.empty (map ftv l)
    apply s = map (apply s)

type TypeEnv = Map.Map String Scheme

remove :: TypeEnv -> String -> TypeEnv
remove env v = Map.delete v env

instance Types TypeEnv where
    ftv env = ftv (Map.elems env)
    apply s env = Map.map (apply s) env

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t'
    where vars = Set.toList $ ftv t' `Set.difference` ftv env
          t' = strip t

sanitize :: Type -> Scheme
sanitize t = generalize Map.empty $ apply tidyS t'
    where t' = strip t
          tidyV = map (\c -> TV $ TVar [c]) "abcedfghijklmnopqrstuvwxyz"
          vars = Set.toList $ ftv t'
          tidyS = Subst $ Map.fromList $ zip vars tidyV

strip :: Type -> Type
strip (TExp _ c ts) = Type c (map strip ts)
strip (Type c ts) = Type c (map strip ts)
strip t = t

type TI a = State Int a

evalTI :: TI a -> a
evalTI t = evalState t 0

newTyVar :: String -> TI TVar
newTyVar prefix = do
    cur <- get
    put (cur + 1)
    return $ TVar $ prefix ++ show cur

instantiate :: Scheme -> TI Type
instantiate (Scheme v t) = do
    nvars <- mapM (\_ -> newTyVar "i") v
    let s = Subst (Map.fromList (zip v $ map TV nvars))
    return $ apply s t

--zipWith/foldl
zfoldl f i (a:as) (b:bs) = zfoldl f (f i a b) as bs
zfoldl _ i [] _ = i
zfoldl _ i _ [] = i

mgu :: Type -> Type -> Subst
mgu (TExp v1 c1 ts1) r@(TExp v2 c2 ts2)
    | c1 == c2 = mgus ts1 ts2
    | otherwise = topSubst v1 `composeSubst` topSubst v2
mgu l@(Type c1 ts1) r@(Type c2 ts2)
    | c1 == Top || c2 == Top = nullSubst
    | c1 == c2 = mgus ts1 ts2
    | otherwise = throwEx $ "types do not unify " ++ show l ++ " ~ " ++ show r
mgu l@(Type c1 ts1) r@(TExp v c2 ts2)
    | c1 == c2 = mgus ts1 ts2
    | otherwise = topSubst v
mgu l@(TExp _ c ts) r@(Type _ _) = mgu r l
mgu (TV v) t = varBind v t
mgu t (TV v) = varBind v t

varBind :: TVar -> Type -> Subst
varBind v t
    | t == TV v = nullSubst
    | v `Set.member` ftv t = topSubst v --this can be turned off to make infinite types
    | otherwise = Subst $ Map.singleton v $ prepare t
    where prepare (Type c ts) = TExp v c ts
          prepare (TExp _ c ts) = TExp v c ts
          prepare tv = tv

mgus :: [Type] -> [Type] -> Subst
mgus [] [] = nullSubst
mgus (t:ts) (u:us) = mgu (apply s1 t) (apply s1 u) `composeSubst` s1
    where s1 = mgus ts us

tiLit :: Value -> TI Type
tiLit (Atom _) = do
    tv <- newTyVar "l"
    return $ TExp tv AtomTy []
tiLit (Number _) = do
    tv <- newTyVar "l"
    return $ TExp tv NumberTy []
tiLit Nil = do
    conts <- newTyVar "l"
    return $ Type List [TV conts]
tiLit (v :. rest) = do
    restT <- tiLit rest
    vT <- tiLit v
    let s = mgu restT $ Type List [vT]
    return $ apply s restT
tiLit _ = error "unexpected run-time value in literal"

ti :: TypeEnv -> Expr -> TI (Subst, Type)

ti _ (ELit l) = do
    litT <- tiLit l
    return (nullSubst, litT)

ti env (EVar n) = --do special types here
    case Map.lookup n env of
        Nothing -> do
            t <- newTyVar n --"u" --throwEx $ "unbound variable " ++ n
            return (nullSubst, TV t)
        Just sigma -> do
            t <- instantiate sigma
            return (nullSubst, t)

ti env (EAbs n e) = do
    tv <- newTyVar "p"
    let env' = Map.insert n (Scheme [] $ TV tv) env --hiding
    (s1, t1) <- ti env' e
    fun <- newTyVar "f"
    return (s1, Type Func [apply s1 (TV tv), t1])

ti env (EApp f a) = do
    ret <- newTyVar "r"
    (s1, t1) <- ti env f
    (s2, t2) <- ti (apply s1 env) a
    let tfun = Type Func [t2, TV ret]
        s3 = mgu (apply s2 t1) tfun
    return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 (TV ret))

ti env (ELet bs e2) = do
    error "not implemented"
{-
ti env (ELetRec bs e2) = do
    nvars <- mapM (\_ -> newTyVar "lr") bs
    let vars = map (\(TVar n) -> n) nvars
        env' = Map.fromList (zip (map var bs) nvars) `Map.union` env
    terms <- mapM (doBind env') bs
    let selfs = Map.fromList $ zip vars $ map snd terms
        alls = map (unifySubst selfs) $ map fst terms
        selfs' = Map.map (generalize env) $ composeSubsts $ zipWith mapOne vars alls
        others = Map.filterWithKey (\k _ -> not (k `elem` vars)) $ composeSubsts alls
        s1 = others `composeSubst` selfs'
    (s2, t2) <- ti (apply s1 env') e2
    return (s2 `composeSubst` s1, t2)
    where doBind env (Binding x e1) = do
              (s, t) <- ti env e1
              return (s, t)
          mapOne k m = Map.singleton k (m Map.! k)
          var (Binding x _) = 
          -}

ti env (ECase alts) = do
    ty <- newTyVar "c" >>= return . TV
    condSubst <- doConds alts nullSubst
    doAlts alts condSubst ty
    where doAlts ((Alt _ e):alts) s t = do
              (s1, t1) <- ti (apply s env) e
              let s2 = mgu t1 (apply (traceV "s1" s1) t)
              doAlts alts (s2 `composeSubst` s1 `composeSubst` s) (apply s2 t1)
          doAlts [] s t = return (s, t)
          doConds ((Alt c _):alts) s = do
              (s1, _) <- ti (apply s env) c
              doConds alts (s1 `composeSubst` s)
          doConds [] s = return s

infer :: TypeEnv -> Expr -> Type
infer env e = evalTI $ do
    (s, t) <- ti env e
    return (apply s t)

inferT :: TypeEnv -> Expr -> IO ()
inferT env e = putStrLn $ show s ++ "\n" ++ show t ++ "\n" ++ show (sanitize t) ++ "\n"
    where (s, t) = evalTI $ ti env e
