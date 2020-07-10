{-# LANGUAGE GADTs #-}

module TypeEval where

import AST
import RandomState


typingValue :: Value -> Type
typingValue (C _) = TColl
typingValue (I _) = TInt
typingValue (B _) = TBool

typingExp :: Expression a -> Type
typingExp (D _ _)   = TColl
typingExp (Z _ _)   = TColl
typingExp (INT _)   = TInt
typingExp (COLL _)  = TColl
typingExp (Var var)   = TColl -- Va a haber que hacer un typingExp monádico
typingExp (Least _ _)  = TColl
typingExp (Largt _ _)  = TColl
typingExp (Filter _ _) = TColl
typingExp (Concat _ _) = TColl
typingExp (MAX _) = TInt
typingExp (MIN _) = TInt
typingExp (SUM _) = TInt
typingExp (COUNT _) = TInt
typingExp (ADD _ _) = TInt
typingExp (MINUS _ _) = TInt
typingExp (TIMES _ _) = TInt
typingExp (DIV _ _) = TInt
typingExp (MOD _ _) = TInt
typingExp (UMINUS _) = TInt
typingExp (SGN _) = TInt
typingExp (INDEP _ _) = TColl
typingExp (BOOL _) = TBool
typingExp (Eq _ _) = TBool
typingExp (NEq _ _) = TBool
typingExp (Lt _ _) = TBool
typingExp (Gt _ _) = TBool
typingExp (GEt _ _) = TBool
typingExp (LEt _ _) = TBool
typingExp (IsEmpty _) = TBool
typingExp (AND _ _) = TBool
typingExp (OR _ _) = TBool
typingExp (NOT _) = TBool

