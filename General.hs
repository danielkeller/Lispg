{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}

module General (
    Env(..),
    Value(..),
    File,
    Binding(..),
    Expr(..),
    Alt(..),
    CFile,
    CBinding(..),
    Cps(..),
    CAlt(..),
    Inline(..),
    Cont(..),
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

type File = [Binding]
data Binding = Binding String Expr
    deriving Show
data Expr = ELit Value
          | EVar String
          | EAbs String Expr
          | EApp Expr Expr
          | ELet [Binding] Expr
          | ELetRec [Binding] Expr
          | ECase [Alt]
    deriving Show
data Alt = Alt Expr Expr
    deriving Show

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

--this operation is only defined on atoms and numbers
instance Eq Value where
    (Atom l) == (Atom r) = l == r
    (Number l) == (Number r) = l == r
    Nil == Nil = True
    _ == _ = False

class Code a where
    freeIn :: a -> Set.Set String --free variables

instance Code Expr where
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

instance Code Cps where
    freeIn (Call l r c) = freeIn l `Set.union` freeIn r `Set.union` freeIn c
    freeIn (CCC v) = freeIn v
    freeIn (Case alts) = Set.unions $ map aFree alts
        where aFree (CAlt c e) = freeIn c `Set.union` freeIn e

instance Code Inline where
    freeIn (Inline e) = freeIn e
    freeIn (Fun p e) = Set.delete p $ freeIn e

instance Code Cont where
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

printExpr :: Expr -> String
printExpr (ELit l) = '\'' : printValue l
printExpr (EVar s) = s
printExpr (EAbs a e) = "λ" ++ a ++ "." ++ printExpr e
printExpr (ELet b e) = "let " ++ intercalate "\n    " (printBindings b) ++ "\n    in " ++ printExpr e
printExpr (ELetRec b e) = printExpr $ ELet b e
printExpr (EApp l r@(EApp _ _)) = printExpr l ++ " (" ++ printExpr r ++ ")"
printExpr (EApp l r@(EAbs _ _)) = printExpr l ++ " (" ++ printExpr r ++ ")"
printExpr (EApp l r) = printExpr l ++ " " ++ printExpr r
printExpr (ECase alts) = "case\n" ++ intercalate "\n" (map printCase alts)
    where printCase (Alt c e) = "    | " ++ printExpr c ++ " -> " ++ printExpr e

printBindings (Binding v e : bs) = (v ++ " = " ++ printExpr e) : printBindings bs
printBindings [] = []

printFile f = intercalate "\n\n" (printBindings f) ++ "\n\n"

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
