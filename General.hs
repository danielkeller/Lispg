{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}

module General (
    Env(..),
    Value(..),
    File,
    Binder(..),
    GBinding(..),
    GExpr(..),
    GAlt(..),
    Binding,
    Expr,
    Alt,
    CFile,
    GCBinding(..),
    GCps(..),
    GCAlt(..),
    GInline(..),
    GCont(..),
    CBinding,
    Cps,
    CAlt,
    Inline,
    Cont,
    InterpreterException(..),
    throwEx,
    traceV,
    Code(..),
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Exception
import Data.Typeable
import Data.List(intercalate, intersperse)

import Debug.Trace
traceV str v = trace (str ++ " " ++ show v) v

data InterpreterException = InterpreterException String
    deriving (Show, Typeable)
instance Exception InterpreterException
throwEx = throw . InterpreterException

type Env = Map.Map String Value

data Value = Atom String | Number Integer | Value :. Value | Nil --compile-time
             | Closure Expr Env | Builtin (Value -> Value) --run-time

infixr 5 :.

--Expressions are parameterized over the type of the binder. This allows us to switch from
--strings to closure offsets in the code generator while maintaining the same code structure
class Binder a where
    printBdr :: a -> ShowS
    contVar :: a

instance Binder String where
    printBdr = showString
    contVar = "k"

data GBinding a = Binding a (GExpr a)
data GExpr a = ELit Value
          | EVar a
          | EAbs a (GExpr a)
          | EApp (GExpr a) (GExpr a)
          | ELet [GBinding a] (GExpr a)
          | ELetRec [GBinding a] (GExpr a)
          | ECase [GAlt a]
data GAlt a = Alt (GExpr a) (GExpr a)

type File = [Binding]
type Binding = GBinding String
type Expr = GExpr String
type Alt = GAlt String

data GCBinding a = CBinding a Inline
data GCps a = Call (GInline a) (GInline a) (GCont a) | CCC (GInline a) | Case [GCAlt a]
data GCAlt a = CAlt (GInline a) (GCps a)
data GInline a = Inline (GExpr a) | Fun a (GCps a)
data GCont a = Cont a (GCps a) -- | CC

type CFile = [CBinding]
type CBinding = GCBinding String
type Cps = GCps String
type CAlt = GCAlt String
type Inline = GInline String
type Cont = GCont String

--this operation is only defined on atoms and numbers
instance Eq Value where
    (Atom l) == (Atom r) = l == r
    (Number l) == (Number r) = l == r
    Nil == Nil = True
    _ == _ = False

class Code a where
    freeIn :: Ord b => a b -> Set.Set b --free variables

instance Code GExpr where
    freeIn (ELit _) = Set.empty
    freeIn (EVar x) = Set.singleton x
    freeIn (EAbs p e) = Set.delete p $ freeIn e
    freeIn (EApp l r) = freeIn l `Set.union` freeIn r
    freeIn (ELet bs ex) = foldl dropB (freeIn ex) bs `Set.union` Set.unions (map bFree bs)
        where bFree (Binding _ e) = freeIn e
              dropB frees (Binding v _) = Set.delete v frees
    freeIn (ELetRec bs ex) = foldl dropB (freeIn ex `Set.union` Set.unions (map bFree bs)) bs
        where bFree (Binding _ e) = freeIn e
              dropB frees (Binding v _) = Set.delete v frees
    freeIn (ECase alts) = Set.unions $ map aFree alts
        where aFree (Alt c e) = freeIn c `Set.union` freeIn e

instance Code GCps where
    freeIn (Call l r c) = freeIn l `Set.union` freeIn r `Set.union` freeIn c
    freeIn (CCC v) = freeIn v
    freeIn (Case alts) = Set.unions $ map aFree alts
        where aFree (CAlt c e) = freeIn c `Set.union` freeIn e

instance Code GInline where
    freeIn (Inline e) = freeIn e
    freeIn (Fun p e) = Set.delete p $ freeIn e

instance Code GCont where
    freeIn (Cont p e) = Set.delete p $ freeIn e

instance Show Value where
    show v = showsPrec 1 v "" --include outer parens
    showsPrec d (l :. r) = showParen (d>0) $ showsPrec 1 l . space . showsPrec 0 r
        where space = if r == Nil then showString "" else showString " "
    showsPrec d Nil = if d>0 then showString "()" else showString ""
    showsPrec d (Atom s) = showString s
    showsPrec d (Number n) = showsPrec 0 n
    showsPrec _ _ = showString "#<closure>"

instance Binder a => Show (GExpr a) where
    showsPrec _ (ELit l) = showString "'" . showsPrec 1 l
    showsPrec _ (EVar b) = printBdr b
    showsPrec d (EAbs b e) = showParen (d>0) $ showString "λ" . printBdr b . showString "." . showsPrec 0 e
    showsPrec d (EApp l r) = showParen (d>1) $ showsPrec 1 l . showString " " . showsPrec 2 r
    showsPrec d (ELet bs e) = showString "let " .
                              foldr1 (.) (intersperse (showString "\n    ") $ map (showsPrec 0) bs) . 
                              showString "\n    in " . showsPrec 0 e
    showsPrec _ (ELetRec b e) = showsPrec 0 (ELet b e)
    showsPrec _ (ECase alts) = showString "case" . foldr1 (.) (map showAlt alts)
        where showAlt (Alt c e) = showString "\n    | " . showsPrec 0 c . showString " -> " . showsPrec 0 e

instance Binder a => Show (GBinding a) where
    showsPrec _ (Binding b e) = printBdr b . showString " = " . showsPrec 0 e

instance Binder a => Show (GCps a) where
    showsPrec d e = showsPrec d $ toExpr e

toExpr :: Binder a => GCps a -> GExpr a
toExpr (Call f p (Cont s c)) = EApp (EApp (fromInline f) (fromInline p)) $ EAbs s (toExpr c)
toExpr (CCC e) = EApp (EVar contVar) $ fromInline e
toExpr (Case a) = ECase (map teAlt a)
    where teAlt (CAlt (Inline c) e) = Alt c (toExpr e)

fromInline :: Binder a => GInline a -> GExpr a
fromInline (Inline e) = e
fromInline (Fun p b) = EAbs p (EAbs contVar (toExpr b))
