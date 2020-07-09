{-# LANGUAGE GADTs #-}

module TypeEval where

import AST


typingValue :: Expression a -> Type
typingValue (Roll _) = TColl 
typingValue (I _)    = TInt
typingValue (C _)    = TColl
typingValue (Var _)  = TColl -- HAS TO SOLVE THE TYPE VAR
typingValue (Least _ _)  = TColl
typingValue (Largt _ _)  = TColl
typingValue (Filter _ _) = TColl
typingValue (Concat _ _) = TColl
typingValue (MAX _) = TInt
typingValue (MIN _) = TInt
typingValue (SUM _) = TInt
typingValue (COUNT _) = TInt
typingValue (ADD _ _) = TInt
typingValue (MINUS _ _) = TInt
typingValue (TIMES _ _) = TInt
typingValue (DIV _ _) = TInt
typingValue (MOD _ _) = TInt
typingValue (UMINUS _) = TInt
typingValue (SGN _) = TInt
typingValue (INDEP _ _) = TColl

