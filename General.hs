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
    printValue,
    printExpr,
    printFile,
    traceV,
    printCps,
    printCpsFile,
    Code(..),
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Exception
import Data.Typeable
import Data.List(intercalate)

import Debug.Trace
traceV str v = trace (str ++ " " ++ show v) v

data InterpreterException = InterpreterException String
    deriving (Show, Typeable)
instance Exception InterpreterException
throwEx = throw . InterpreterException

type Env = Map.Map String Value

data Value = Atom String | Number Integer | Value :. Value | Nil --compile-time
             | Closure Expr Env | Builtin (Value -> Value) --run-time
    deriving Show

instance Show (Value -> Value) where
    --this will probably crash if you try to use it

infixr 5 :.

--Expressions are parameterized over the type of the binder. This allows us to switch from
--strings to closure offsets in the code generator while maintaining the same code structure
class Binder a where
    printBdr :: a -> String
    contVar :: a

instance Binder String where
    printBdr = id
    contVar = "k"

data GBinding a = Binding a (GExpr a)
    deriving Show
data GExpr a = ELit Value
          | EVar a
          | EAbs a (GExpr a)
          | EApp (GExpr a) (GExpr a)
          | ELet [GBinding a] (GExpr a)
          | ELetRec [GBinding a] (GExpr a)
          | ECase [GAlt a]
    deriving Show
data GAlt a = Alt (GExpr a) (GExpr a)
    deriving Show

type File = [Binding]
type Binding = GBinding String
type Expr = GExpr String
type Alt = GAlt String

data GCBinding a = CBinding a Inline
    deriving Show
data GCps a = Call (GInline a) (GInline a) (GCont a) | CCC (GInline a) | Case [GCAlt a]
    deriving Show
data GCAlt a = CAlt (GInline a) (GCps a)
    deriving Show
data GInline a = Inline (GExpr a) | Fun a (GCps a)
    deriving Show
data GCont a = Cont a (GCps a) -- | CC
    deriving Show

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

printValue :: Value -> String
printValue l@(_:._) = "(" ++ plHelp l ++ ")"
printValue (Closure _ _) = "#<closure>"
printValue (Builtin _) = "#<closure>"
printValue l = plHelp l
plHelp Nil = "()"
plHelp (Atom s) = s
plHelp (Number n) = show n
plHelp (l@(_:._) :. Nil) = "(" ++ plHelp l ++ ")"
plHelp (l@(_:._) :. r@(_ :. _)) = "(" ++ plHelp l ++ ") " ++ plHelp r
plHelp (l@(_:._) :. r) = "(" ++ plHelp l ++ ") . " ++ plHelp r
plHelp (l :. Nil) = plHelp l
plHelp (l :. r@(_ :. _)) = plHelp l ++ " " ++ plHelp r
plHelp (l :. r) = plHelp l ++ " . " ++ plHelp r

printExpr :: Binder a => GExpr a -> String
printExpr (ELit l) = '\'' : printValue l
printExpr (EVar s) = printBdr s
printExpr (EAbs a e) = "λ" ++ printBdr a ++ "." ++ printExpr e
printExpr (ELet b e) = "let " ++ intercalate "\n    " (printBindings b) ++ "\n    in " ++ printExpr e
printExpr (ELetRec b e) = printExpr $ ELet b e
printExpr (EApp l r@(EApp _ _)) = printExpr l ++ " (" ++ printExpr r ++ ")"
printExpr (EApp l r@(EAbs _ _)) = printExpr l ++ " (" ++ printExpr r ++ ")"
printExpr (EApp l r) = printExpr l ++ " " ++ printExpr r
printExpr (ECase alts) = "case\n" ++ intercalate "\n" (map printCase alts)
    where printCase (Alt c e) = "    | " ++ printExpr c ++ " -> " ++ printExpr e

printBindings (Binding v e : bs) = (printBdr v ++ " = " ++ printExpr e) : printBindings bs
printBindings [] = []

printFile f = intercalate "\n\n" (printBindings f) ++ "\n\n"

toExpr :: Binder a => GCps a -> GExpr a
toExpr (Call f p (Cont s c)) = EApp (EApp (fromInline f) (fromInline p)) $ EAbs s (toExpr c)
toExpr (CCC e) = EApp (EVar contVar) $ fromInline e
toExpr (Case a) = ECase (map teAlt a)
    where teAlt (CAlt (Inline c) e) = Alt c (toExpr e)

fromInline :: Binder a => GInline a -> GExpr a
fromInline (Inline e) = e
fromInline (Fun p b) = EAbs p (EAbs contVar (toExpr b))

printCps :: Binder a => GCps a -> String
printCps = printExpr . toExpr
printCpsFile = concat . map printB
    where printB (CBinding s e) = s ++ " = " ++ printExpr (fromInline e) ++ "\n"
