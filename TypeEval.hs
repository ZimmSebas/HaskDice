{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}


module TypeEval where

import AST
import RandomState
import System.Random 


-- Check if a program is typed correctly
evalType :: Command a -> Maybe Type
evalType exp = case (runTS (do { res <- typingCommand exp; return res}) initStateType) of
              Nothing      -> Nothing
              Just (t,_) -> Just t

-- Checks if two types are equal
equalType :: (MonadState m Type, MonadError m) => Type -> Type -> m ()
equalType t1 t2 = if (t1 == t2) then return () 
                                else throw -- i should say expecting type1 found type2 in expression e

-- Checks if in a binary operator that expects types t1/t2, the actual types are correct
typingBinaryOp :: (MonadState m Type, MonadError m) => Type -> Expression a -> Type -> Expression b -> Type -> m Type
typingBinaryOp t1 e1 t2 e2 optype = do
    actt1 <- typingExp e1
    equalType t1 actt1 --maybe pass e1 as argument?
    actt2 <- typingExp e2
    equalType t2 actt2
    return optype

-- Checks if a unaryOp that expects type t1 has the actual type t1. 
typingUnaryOp :: (MonadState m Type, MonadError m) => Type -> Expression a -> Type -> m Type
typingUnaryOp typ exp untype = do
    acttyp <- typingExp exp
    equalType typ acttyp
    return untype

-- Checks for a Value what type it is.
typingValue :: (MonadState m Type, MonadError m) => Value -> m Type
typingValue (C _) = return TColl
typingValue (I _) = return TInt
typingValue (B _) = return TBool

-- Checks if type of arguments of an expression are correct, and returns the type of the expression
typingExp :: (MonadState m Type, MonadError m) => Expression a -> m Type
typingExp (D _ _)   = return TColl
typingExp (Z _ _)   = return TColl
typingExp (INT _)   = return TInt
typingExp (COLL _)  = return TColl
typingExp (Least _ c)  = typingUnaryOp TColl c TColl
typingExp (Largt _ c)  = typingUnaryOp TColl c TColl
typingExp (Filter _ c) = typingUnaryOp TColl c TColl
typingExp (Concat c1 c2) = typingBinaryOp TColl c1 TColl c2 TColl
typingExp (MAX c) = typingUnaryOp TColl c TInt
typingExp (MIN c) = typingUnaryOp TColl c TInt
typingExp (SUM c) = typingUnaryOp TColl c TInt
typingExp (COUNT c) = typingUnaryOp TColl c TInt
typingExp (ADD x y) = typingBinaryOp TInt x TInt y TInt
typingExp (MINUS x y) = typingBinaryOp TInt x TInt y TInt
typingExp (TIMES x y) = typingBinaryOp TInt x TInt y TInt
typingExp (DIV x y) = typingBinaryOp TInt x TInt y TInt
typingExp (MOD x y) = typingBinaryOp TInt x TInt y TInt
typingExp (UMINUS x) = typingUnaryOp TInt x TInt
typingExp (SGN x) = typingUnaryOp TInt x TInt
typingExp (INDEP n c) = typingBinaryOp TInt n TColl c TColl
typingExp (BOOL _) = return TBool
typingExp (Eq x y) = typingBinaryOp TInt x TInt y TBool
typingExp (NEq x y) = typingBinaryOp TInt x TInt y TBool
typingExp (Lt x y) = typingBinaryOp TInt x TInt y TBool
typingExp (Gt x y) = typingBinaryOp TInt x TInt y TBool
typingExp (GEt x y) = typingBinaryOp TInt x TInt y TBool
typingExp (LEt x y) = typingBinaryOp TInt x TInt y TBool
typingExp (AND p q) = typingBinaryOp TBool p TBool q TBool
typingExp (OR p q) = typingBinaryOp TBool p TBool q TBool
typingExp (NOT p) = typingUnaryOp TBool p TBool
typingExp (IsEmpty v) = do
    typingValue v
    return TBool
typingExp (Var var) = do
    t <- lookfor var 
    return t

-- Takes a command, checks the type of the result and if some command has typing errors.
typingCommand :: (MonadState m Type, MonadError m) => Command a -> m Type
typingCommand (Expr e) = typingExp e
typingCommand (Let v e) = do
        t <- typingExp e
        update v t
        return t
typingCommand (Seq c1 c2) = do 
        tc1 <- typingCommand c1
        tc2 <- typingCommand c2
        return tc2
typingCommand (IfThenElse b c1 c2) = do 
        tb <- typingUnaryOp TBool b TBool
        tc1 <- typingCommand c1
        tc2 <- typingCommand c2
        return tc2
typingCommand (ACCUM c1 c2) = do
        tc1 <- typingCommand c1
        tc2 <- typingCommand c2
        return tc1
typingCommand (REPUNT c1 c2) = do
        tc1 <- typingCommand c1
        tc2 <- typingCommand c2
        return tc1        
