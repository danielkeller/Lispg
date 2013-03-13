module Cps (
    toCps,
    fileToCps,
    printCps,
    printCpsFile,
    CFile,
    CBinding(..),
    Cps(..),
    CAlt(..),
    Inline(..),
    Cont(..)
) where

import General
import Builtins
import Control.Monad.State
import qualified Data.Map as Map

type CFile = [CBinding]
data CBinding = CBinding String Inline
    deriving Show

data Cps = Call Inline Inline Cont | CCC Inline | Case [CAlt]
    deriving Show
data CAlt = CAlt Inline Cps
    deriving Show
data Inline = Inline Expr | Fun String Cps
    deriving Show
data Cont = Cont String Cps -- | CC
    deriving Show

toExpr :: Cps -> Expr
toExpr (Call f p (Cont s c)) = EApp (EApp (fromInline f) (fromInline p)) $ EAbs s (toExpr c)
toExpr (CCC e) = EApp (EVar "k") $ fromInline e
toExpr (Case a) = ECase (map teAlt a)
    where teAlt (CAlt (Inline c) e) = Alt c (toExpr e)

fromInline :: Inline -> Expr
fromInline (Inline e) = e
fromInline (Fun p b) = EAbs p (EAbs "k" (toExpr b))

printCps = printExpr . toExpr
printCpsFile = concat . map printB
    where printB (CBinding s e) = s ++ " = " ++ printExpr (fromInline e) ++ "\n"

toCps :: Expr -> Cps
toCps e = evalV $ do
    (c, _) <- toCps' e
    return c

fileToCps :: File -> CFile
fileToCps f = map toCpsBinding f
    where toCpsBinding (Binding s e) = CBinding s $ doBind $ toCps e
          doBind (CCC ex) = ex

type V a = State Int a

evalV :: V a -> a
evalV t = evalState t 0

newVar :: V String
newVar = do
    cur <- get
    put (cur + 1)
    return $ "c" ++ show cur

inlines = Map.keys builtins

shouldInline :: Expr -> Bool
shouldInline (EApp (EVar l) r) = l `elem` inlines && shouldInline r
shouldInline (EApp l r) = shouldInline l && shouldInline r
shouldInline (EVar _) = True
shouldInline (ELit _) = True
shouldInline _ = False

ev str = Inline $ EVar str

--append function for Cps
andThen :: Cps -> Cps -> Cps
andThen (Call l r (Cont s k)) k1 = Call l r (Cont s (k `andThen` k1))
andThen (CCC e) k1 = k1

andThenCont :: Cps -> Cont -> Cps
andThenCont (Call l r (Cont s (CCC e))) k1 = Call l r k1
andThenCont (Call l r (Cont s k)) k1 = Call l r (Cont s (k `andThenCont` k1))
andThenCont (CCC e) k1 = Call (ev "<letk>") e k1

makeCall :: Inline -> Inline -> V (Cps, Inline)
makeCall ll@(Inline l) rr@(Inline r)
    | shouldInline l = return (CCC $ Inline $ EApp l r, Inline $ EApp l r)
    | otherwise = makeNICall ll rr
makeCall l r = makeNICall l r

makeNICall l r = do
        contv <- newVar
        let res = Inline $ EVar contv
        let ret = Call l r $ Cont contv $ CCC res
        return (ret, res)

toCps' :: Expr -> V (Cps, Inline)
toCps' i@(EApp l r) = if shouldInline i
    then return (CCC $ Inline i, Inline i)
    else do
        (lhs, lname) <- toCps' l
        (rhs, rname) <- toCps' r
        (ret, res) <- makeCall lname rname
        return (lhs `andThen` rhs `andThen` ret, res)
toCps' (EAbs p e) = do
    (c, _) <- toCps' e
    let ret = Fun p c
    return (CCC ret, ret)
toCps' (ELet (Binding s e : bs) e1) = do
    (bind, bname) <- toCps' e
    (expr, exname) <- toCps' $ ELet bs e1
    let ret = bind `andThenCont` Cont s expr
    return (ret, exname)
toCps' (ELet [] e) = toCps' e
toCps' (ECase alts) = do
    alts' <- mapM (\ (Alt c e) -> do
        (e', n) <- toCps' e
        return $ CAlt (Inline c) e'
        ) alts
    return (Case alts', ev "FIXME")
toCps' i = return (CCC $ Inline i, Inline i)
