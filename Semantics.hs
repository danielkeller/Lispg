module Semantics (
    deSugar,
    dsExpr,
    printExpr,
    printFile,
) where

import Syntax
import General
import Data.List(intercalate)

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

lispTrue = ELit (Atom "#t")
lispFalse = ELit (Atom "#f")

deSugar :: Value -> File
deSugar Nil = []
deSugar (bind :. rest) = dsBinding bind : deSugar rest

dsBinding (Atom "define" :. Atom var :. binding :. Nil) = Binding var $ dsExpr binding
dsBinding x = throwEx $ "malformed binding " ++ printValue x

--allow mapping over scheme lists
scmMap :: (Value -> a) -> Value -> [a]
scmMap f (l :. r) = f l : scmMap f r
scmMap _ Nil = []
scmMap _ x = throwEx $ printValue x ++ " is not a list"

dsBind (Atom v :. expr :. Nil) = Binding v $ dsExpr expr
dsBind bad = throwEx $ "malformed binding " ++ printValue bad

dsExpr all@(Atom "lambda" :. args :. body :. Nil) = dsLam args body
    where dsLam Nil body = dsExpr body
          dsLam (Atom a :. as) body = EAbs a $ dsLam as body
          dsLam _ _ = throwEx $ "malformed lambda " ++ printValue all

dsExpr bad@(Atom "lambda" :. _) = throwEx $ "malformed lambda " ++ printValue bad

dsExpr (Atom "if" :. test :. body1 :. body2 :. Nil) =
    ECase [Alt (dsExpr test) (dsExpr body1), Alt lispTrue (dsExpr body2)]

dsExpr bad@(Atom "if" :. _) = throwEx $ "malformed if " ++ printValue bad

dsExpr (Atom "cond" :. body) = ECase $ scmMap dsAlt body
    where dsAlt (c :. b :. Nil) = Alt (dsExpr c) (dsExpr b)
          dsAlt bad = throwEx $ "malformed condition " ++ printValue bad

dsExpr (Atom "let" :. bindings :. expr :. Nil) = ELet (scmMap dsBind bindings) (dsExpr expr)
dsExpr (Atom "let" :. bad) = throwEx $ "malformed let " ++ printValue bad

dsExpr (Atom "let*" :. bindings :. expr :. Nil) = dsBind' bindings
    where dsBind' (b :. bs) = ELet [dsBind b] (dsBind' bs)
          dsBind' Nil = dsExpr expr
          dsBind' bad = throwEx $ "malformed binding " ++ printValue bad

dsExpr (Atom "let*" :. bad) = throwEx $ "malformed let* " ++ printValue bad

dsExpr (Atom "letrec" :. bindings :. expr :. Nil) = ELetRec (scmMap dsBind bindings) (dsExpr expr)
dsExpr (Atom "letrec" :. bad) = throwEx $ "malformed letrec " ++ printValue bad

dsExpr (Atom "quote" :. expr :. Nil) = ELit expr
--don't need well-formedness check, this is generated by parser

--function call
dsExpr (fun :. args) = dsFun (dsExpr fun) args
    where dsFun ex (arg :. args) = dsFun (EApp ex (dsExpr arg)) args
          dsFun ex Nil = ex

dsExpr lit@(Atom l)
    | head l == '#' = ELit lit
    | l == "else" = lispTrue
    | otherwise = EVar l
dsExpr l@(Number _) = ELit l

dsExpr Nil = ELit Nil

dsExpr x = throwEx $ "internal error - unexpected form \n" ++ printValue x
