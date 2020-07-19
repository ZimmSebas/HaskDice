{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}


module TypeEval where

import AST
import RandomState
import System.Random 

-- |initState of type enviroment 
initStateType ::TypEnv
initStateType = []

-- Takes a variable and transforms and returns the type (with the name of the variable)
varToType :: (Variable,Value) -> (Variable,Type)
varToType (str,(C _)) = (str,TColl)
varToType (str,(I _)) = (str,TInt)
varToType (str,(B _)) = (str,TBool)

-- Mapfunction to map an enviroment of variables and returns the type.
mapingTypeEnv :: Env -> TypEnv
mapingTypeEnv state = map varToType state


-- Checks if two types are equal
equalType :: (MonadState m Type, MonadError m) => Type -> Type -> String -> m ()
equalType t1 t2 e = if (t1 == t2) then return () 
                                  else throwTypingError t1 t2 e 

-- Checks if in a binary operator that expects types t1/t2, the actual types are correct
typingBinaryOp :: (MonadState m Type, MonadError m) => Type -> Expression -> Type -> Expression -> Type -> m Type
typingBinaryOp t1 e1 t2 e2 optype = do
    actt1 <- typingExp e1
    equalType t1 actt1 (show e1) 
    actt2 <- typingExp e2
    equalType t2 actt2 (show e1)
    return optype

-- Checks if a unaryOp that expects type t1 has the actual type t1. 
typingUnaryOp :: (MonadState m Type, MonadError m) => Type -> Expression -> Type -> m Type
typingUnaryOp typ exp untype = do
    acttyp <- typingExp exp
    equalType typ acttyp (show exp)
    return untype

-- Checks if a unaryOp that expects type t1 has the actual type t1. 
typingUnaryCommand :: (MonadState m Type, MonadError m) => Type -> Command -> Type -> m Type
typingUnaryCommand typ com untype = do
    acttyp <- typingCommand com
    equalType typ acttyp (show com)
    return untype


-- Checks for a Value what type it is.
typingValue :: (MonadState m Type, MonadError m) => Value -> m Type
typingValue (C _) = return TColl
typingValue (I _) = return TInt
typingValue (B _) = return TBool

-- Checks if type of arguments of an expression are correct, and returns the type of the expression
typingExp :: (MonadState m Type, MonadError m) => Expression -> m Type
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
typingCommand :: (MonadState m Type, MonadError m) => Command -> m Type
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
        tc1 <- typingUnaryCommand TColl c1 TColl
        tc2 <- typingCommand c2
        return tc1
typingCommand (REPUNT c1 c2) = do
        tc1 <- typingUnaryCommand TColl c1 TColl
        tc2 <- typingCommand c2
        return tc1        

-------------------------------------
--- Typing Evaluator ----------------
-------------------------------------


-- Check if a program is typed correctly
evalType :: Command -> Env -> Result Type
evalType exp st = case (runTS (do { res <- typingCommand exp; return res}) (mapingTypeEnv st)) of
              Crash e      -> Crash e
              Return (t,_) -> Return t
