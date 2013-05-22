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

--type expansion
data TExp = TExp Constr [TVar]
    deriving Eq

data Type = Type Constr [Type]
          | TV TVar
          deriving Eq

data Scheme = Scheme [TVar] Type --universal quantification
    deriving Eq

instance Show TVar where
    showsPrec _ (TVar s) = showString s

instance Show TExp where
    showsPrec d (TExp c a) = showsPrec d $ Type c $ map TV a

instance Show Type where
    showsPrec d (TV v) = showsPrec d v
    showsPrec d (Type Func [a,r]) = showParen (d>5) $
        showsPrec 6 a . showString " -> " . showsPrec 5 r
    showsPrec _ (Type List [c]) = showString "[" . showsPrec 0 c . showString "]"
    showsPrec _ (Type Top _) = showString "⊤"
    showsPrec _ (Type a _) = showsPrec 0 a

instance Show Scheme where
    showsPrec _ (Scheme free t) = showString ("∀ " ++ intercalate "," (map show free) ++ " . ") . showsPrec 0 t

data Subst = Subst (Map.Map TVar TVar) (Map.Map TVar TExp)

instance Show Subst where
    showsPrec _ (Subst v e) = showListWith printS (Map.toAscList v)
        . showString ", "
        . showListWith printS (Map.toAscList e) --oh hey let-polymorphism is useful.
        where printS (var, t) = showsPrec 0 var . showString " => " . showsPrec 0 t

nullSubst :: Subst
nullSubst = Subst Map.empty Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst a@(Subst v1 e1) b@(Subst v2 e2) = Subst v3 e3
    where v3 = Map.map (\v -> lookupOr v v1 v) v2 `Map.union` v1
          e3 = Map.unionWith ovr e1 e2
          ovr l@(TExp Top _) _ = l
          ovr _ r@(TExp Top _) = r
          ovr b1 b2
              | b1 == b2 = b1
              |otherwise = error $ "composition conflict " ++ show b1 ++ " ~ " ++ show b2
composeSubsts :: [Subst] -> Subst
composeSubsts = foldl composeSubst nullSubst

class Types a where
    ftv :: a -> Set.Set TVar
    apply :: Subst -> a -> a

instance Types Type where
    ftv (TV v) = Set.singleton v
    ftv (Type _ ts) = Set.unions $ map ftv ts
    apply s t = if av == t && af == t then t else apply s af
        where av = applyV s t
              af = applyF s av
              applyV (Subst vs _) (TV v) = TV $ lookupOr v vs v
              applyV _ (Type c as) = Type c $ map (applyV s) as
              applyF (Subst _ es) (TV v) = case Map.lookup v es of
                  Just (TExp c as) -> Type c $ map TV as
                  Nothing -> TV v
              applyF _ (Type c as) = Type c $ map (applyF s) as

instance Types TExp where
    ftv (TExp _ vs) = Set.fromList vs
    apply (Subst vs _) (TExp c as) = TExp c $ map (\a -> lookupOr a vs a) as

instance Types Scheme where
    ftv (Scheme v t) = ftv t `Set.difference` Set.fromList v
    apply (Subst vs es) (Scheme v t) = Scheme v $ apply (Subst (delAll vs v) (delAll es v)) t
        where delAll m l = foldr Map.delete m l

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
generalize env t = Scheme vars t
    where vars = Set.toList $ ftv t `Set.difference` ftv env

type TI a = State Int a

evalTI :: TI a -> a
evalTI t = evalState t 0

newTyVar :: String -> TI TVar
newTyVar prefix = do
    cur <- get
    put (cur + 1)
    return $ TVar $ prefix ++ show cur

instantiate :: Scheme -> TI (Subst, TVar)
instantiate (Scheme v t) = do
    nvars <- mapM (\_ -> newTyVar "i") v
    let s = Subst (Map.fromList (zip v nvars)) Map.empty
    expTy $ apply s t

expTy :: Type -> TI (Subst, TVar)
expTy (Type c ts) = do
    outer <- newTyVar "e"
    rest <- mapM expTy ts
    let thisS = Subst Map.empty $ Map.singleton outer $ TExp c $ map snd rest
    let restS = composeSubsts $ map fst rest
    return (thisS `composeSubst` restS, outer)
expTy (TV v) = return (nullSubst, v)

--zipWith/foldl
zfoldl f i (a:as) (b:bs) = zfoldl f (f i a b) as bs
zfoldl _ i [] _ = i
zfoldl _ i _ [] = i

mgu :: Subst -> TVar -> TVar -> Subst
mgu s@(Subst v e) a b = mgu' t u `composeSubst` s
    where (c, d) = (lookupOr a v a, lookupOr b v b)
          (t, u) = (Map.lookup c e, Map.lookup d e)
          varsub = Subst (if c == d then Map.empty else Map.singleton c d) Map.empty
          mgu' Nothing Nothing = varsub
          mgu' Nothing (Just ty) = varBind c d ty
          mgu' (Just ty) Nothing = varBind d c ty
          mgu' (Just (TExp x as)) (Just (TExp y bs))
              | x == Top || y == Top = nullSubst
              | x == y = zfoldl mgu (varsub `composeSubst` s) as bs
              | otherwise = Subst Map.empty $ Map.fromList [(c, TExp Top []), (d, TExp Top [])]

--Bind u to v, which is also t
varBind :: TVar -> TVar -> TExp -> Subst
varBind u v t
    | u `Set.member` ftv t = Subst Map.empty $ Map.singleton u $ TExp Top [] --this might not be right. check deep recur. types
    | otherwise = Subst (Map.singleton u v) Map.empty

tiLit :: Value -> TI (Subst, TVar)
tiLit (Atom _) = expTy $ Type AtomTy []
tiLit (Number _) = expTy $ Type NumberTy []
tiLit Nil = do
    conts <- newTyVar "l"
    expTy $ Type List [TV conts]
tiLit _ = error "unexpected run-time value in literal"

ti :: TypeEnv -> Expr -> TI (Subst, TVar)

ti env (ELit (v :. rest)) = -- cheat a bit for simplicity
    ti env (EApp (EApp (EVar "cons") (ELit v)) (ELit rest))
ti _ (ELit l) = tiLit l

ti env (EVar n) = --do special types here
    case Map.lookup n env of
        Nothing -> do
            t <- newTyVar n --"u" --throwEx $ "unbound variable " ++ n
            return (nullSubst, t)
        Just sigma -> instantiate sigma

ti env (EAbs n e) = do
    tv <- newTyVar "p"
    let env' = Map.insert n (Scheme [] $ TV tv) env --hiding
    (s1, a1) <- ti env' e
    (s2, a2) <- expTy $ Type Func [TV tv, TV a1]
    return (s2 `composeSubst` s1, a2)

ti env (EApp f a) = do
    ret <- newTyVar "r"
    (s1, a1) <- ti env f
    (s2, a2) <- ti (apply s1 env) a
    (s3, a3) <- expTy $ Type Func [TV a2, TV ret]
    let sa = s3 `composeSubst` s2 `composeSubst` s1
        su = mgu sa a1 a3
    return (su, ret)

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
    tv <- newTyVar "c"
    condSubst <- doConds alts nullSubst
    doAlts alts condSubst tv
    where doAlts ((Alt _ e):alts) s a = do
              (s1, a1) <- ti (apply s env) e
              let s2 = mgu (s1 `composeSubst` s) a a1
              doAlts alts s2 a
          doAlts [] s a = return (s, a)
          doConds ((Alt c _):alts) s = do
              (s1, a1) <- ti (apply s env) c
              doConds alts (s1 `composeSubst` s)
          doConds [] s = return s

infer :: TypeEnv -> Expr -> Type
infer env e = evalTI $ do
    (s, a) <- ti env e
    return (apply s $ TV a)

inferT :: TypeEnv -> Expr -> IO ()
inferT env e = putStrLn $ show s ++ "\n" ++ show a ++ "\n" ++ show (apply s $ TV a) ++ "\n"
    where (s, a) = evalTI $ ti env e
