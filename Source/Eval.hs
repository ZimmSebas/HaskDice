{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Eval where

import AST
import TypeEval
import RandomState
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Data.List
import System.IO
import System.Random 
import Prelude


---------------------------------------
----- Evaluator -----------------------
---------------------------------------


-- Check if a program is typed correctly
evalType :: Command a -> Result Type
evalType exp = case (runTS (do { res <- typingCommand exp; return res}) initStateType) of
              Crash e      -> Crash e
              Return (t,_) -> Return t

-- eval is the first function to be called, to eval the result that the main call upon.
eval :: StdGen -> Command a -> Result (Value,Env)
eval gen exp = case evalType exp of
                    Crash e     -> Crash e
                    Return _    -> case (runRS (do {res <- evalCommand exp; return res}) initState gen) of
                            Crash e              -> Crash e
                            Return (val, st, sg) -> Return (val,st)

-- evalFiltOp eval an operator to the filter, and returns it in a function.

evalFiltOp :: FilOp -> (Int -> Bool)
evalFiltOp (Grtth n)  = (>n)
evalFiltOp (Lowth n)  = (<n)
evalFiltOp (GrtEqt n) = (>=n)
evalFiltOp (LowEqt n) = (<=n)
evalFiltOp (Equal n)  = (==n)
evalFiltOp (NEqual n) = (/=n)

-- evalExp takes any kind of expression and returns the representation.
evalExp :: (MonadState m Value, MonadError m, MonadRandom m) => Expression a -> m Value
evalExp (D k n) = do
    g' <- getStd
    let rolls = take k (randomRs (1 :: Int,n) g')
    return (C rolls)
evalExp (Z k n) = do
    g' <- getStd
    let rolls = take k (randomRs (0 :: Int,n) g')
    return (C rolls)
evalExp (INT n) = return (I n) 
evalExp (COLL c) = return (C c) 
evalExp (Var v) = lookfor v
evalExp (Least k ce) = do
           (C rolls) <- evalExp ce
           let rolls' = take k (sort rolls)
           return (C rolls')
evalExp (Largt k ce) = do
           (C rolls) <- evalExp ce
           let rolls' = take k (sortBy (flip compare) rolls)
           return (C rolls')
evalExp (Filter fop ce) = do
           (C rolls) <- evalExp ce
           let funcfilt = evalFiltOp fop
           return $ C (filter funcfilt rolls)
evalExp (Concat exp1 exp2) = do
           (C c1) <- evalExp exp1
           (C c2) <- evalExp exp2
           return $ C (c1 ++ c2)
evalExp (MAX ce) = do
            (C rolls) <- evalExp ce
            return (I $ foldr max 0 rolls)
evalExp (MIN ce) = do
            (C rolls) <- evalExp ce
            return (I $ foldr min (maxBound::Int) rolls)
evalExp (SUM ce) = do
            (C rolls) <- evalExp ce
            return (I $ sum rolls)
evalExp (COUNT ce) = do
            (C rolls) <- evalExp ce
            return (I $ length rolls)
evalExp (ADD x y) = do
            (I x') <- evalExp x
            (I y') <- evalExp y
            return $ I (x' + y')
evalExp (MINUS x y) = do
            (I x') <- evalExp x
            (I y') <- evalExp y
            return $ I (x' - y')
evalExp (TIMES x y) = do
            (I x') <- evalExp x
            (I y') <- evalExp y
            return $ I (x' * y')
evalExp (DIV x y) = do
            (I x') <- evalExp x
            (I y') <- evalExp y
            if (y' == 0) then throwDivByZero (show x) (show y)
                         else return $ I (x' `div` y')
evalExp (MOD x y) = do
            (I x') <- evalExp x
            (I y') <- evalExp y
            if (y' == 0) then throwModByZero (show x) (show y)
                         else return $ I (x' `mod` y')
evalExp (UMINUS x) = do
            (I n) <- evalExp x
            return $ I (n*(-1))
evalExp (SGN x) = do
            (I n) <- evalExp x
            return $ I (signum n)
evalExp (INDEP n c) = do
            (I cant) <- evalExp n
            (C coll) <- evalExp c
            if (cant > 0) then do {(C coll2) <- evalExp (INDEP (INT (cant-1)) c) ; return $ C (coll ++ coll2)}
                          else return (C coll)
evalExp (BOOL b) = return (B b)
evalExp (IsEmpty v) = do
        case v of
            (C coll) -> return $ B (coll == [])
            (B bool) -> return $ B (bool == True)
            (I numb) -> return $ B (numb == 0)
evalExp (Eq e1 e2) = do
        (I n) <- evalExp e1
        (I m) <- evalExp e2
        return $ B (n == m)
evalExp (NEq e1 e2) = do
        (I n) <- evalExp e1
        (I m) <- evalExp e2
        return $ B (n /= m)
evalExp (Lt e1 e2) = do
        (I n) <- evalExp e1
        (I m) <- evalExp e2
        return $ B (n < m)
evalExp (Gt e1 e2) = do
        (I n) <- evalExp e1
        (I m) <- evalExp e2
        return $ B (n > m)
evalExp (LEt e1 e2) = do
        (I n) <- evalExp e1
        (I m) <- evalExp e2
        return $ B (n <= m)
evalExp (GEt e1 e2) = do
        (I n) <- evalExp e1
        (I m) <- evalExp e2
        return $ B (n <= m)
evalExp (AND b1 b2) = do
        (B x) <- evalExp b1
        (B y) <- evalExp b2
        return $ B (x && y)
evalExp (OR b1 b2) = do
        (B x) <- evalExp b1
        (B y) <- evalExp b2
        return $ B (x || y)
evalExp (NOT b) = do
        (B x) <- evalExp b
        return $ B (not x)

-- evalExp takes a Boolean expression and evaluates the result


-- eval Command takes a command and evaluates the changes in the state.
-- Eval Command returns a Value (Either Collection Int), based on what i had evalued.
-- La cosa es que el eval de commands va a devolver un Value. Entonces devuelve todo junto y que haya un comando Print que printee y listo. Ces't fini.
 
evalCommand :: (MonadState m Value, MonadError m, MonadRandom m) => Command a -> m Value
evalCommand (Expr exp) = do
            e <- evalExp exp
            return e
evalCommand (Seq c1 c2) = do
            n <- evalCommand c1
            m <- evalCommand c2
            return m
evalCommand (IfThenElse b c1 c2) = do
            (B bool) <- evalExp b
            if (bool) then (do {res <- evalCommand c1; return res})
                      else (do {res <- evalCommand c2; return res})
evalCommand (Let name e) = do  
            res <- evalExp e
            update name res
            return res
evalCommand (REPUNT (Let v c) exp) = do
            (C e1) <- evalCommand (Let v c)
            e <- evalCommand exp;
            res <- evalCommand (IfThenElse (IsEmpty e) (Expr (COLL e1)) (REPUNT (Let v c) exp));
            return res
evalCommand (ACCUM (Let v c) exp) = do
            (C head) <- evalCommand (Let v c)
            e <- evalCommand exp;
            (C tail) <- evalCommand (IfThenElse (IsEmpty e) (Expr (COLL [])) (ACCUM (Let v c) exp));
            (C list) <- evalExp (Concat (COLL head) (COLL tail)) 
            return (C list)


-- Voy a tener que implementar booleanos con los REPUNT y ACCUM. Ver cómo hacer eso.
-- Definí un estandar de IsEmpty : Bool = False, Coll = [], Int = 0


main = do  
    g <- newStdGen
    let res = eval g (Expr (Filter (GrtEqt 3) (Largt 3 (D 5 8)) ))
    let typeres = evalType (Expr (Filter (GrtEqt 3) (Largt 3 (D 5 8)) ))
    -- ~ print res
    -- ~ print typeres
    
    let test1 = eval g (Let "x" (COLL [1,2,3]))
    let typetest1 = evalType (Let "x" (COLL [1,2,3]))
    -- ~ print test1
    -- ~ print typetest1
    
    
    let test2 = eval g (IfThenElse (IsEmpty (C [])) (Expr (D 1 6)) (Expr (Z 1 8)))
    let typetest2 = evalType (IfThenElse (IsEmpty (C [])) (Expr (D 1 6)) (Expr (Z 1 8)))
    -- ~ print test2
    -- ~ print typetest2

    let test3 = eval g (Seq (Let "b" (D 2 6)) (IfThenElse (Eq (MAX (Var "b")) (MIN (Var "b"))) (Expr (Concat (Var "b") (Var "b"))) (Expr (Var "b"))))
    let typetest3 = evalType (Seq (Let "b" (D 2 6)) (IfThenElse (Eq (MAX (Var "b")) (MIN (Var "b"))) (Expr (Concat (Var "b") (Var "b"))) (Expr (Var "b"))))
    
    let test4 = eval g (Seq (Let "b" (D 2 6)) (IfThenElse (Eq (MAX (INT 2)) (MIN (Var "b"))) (Expr (Concat (Var "b") (Var "b"))) (Expr (Var "b"))))
    
    let test5 = eval g (ACCUM (Let "b" (D 2 6)) (Expr (Eq (MAX (INT 2)) (MIN (Var "b"))))  )
    let test6 = eval g (Seq (Let "a" (D 1 6)) (Seq (Let "b" (D 1 6)) (Seq (Let "c" (D 1 6)) (Let "d" (D 1 6)) ) ) )
    let testdivZero = eval g (Expr (DIV (SUM (D 1 6)) (INT 0)) )
    let testmodZero = eval g (Expr (MOD (SUM (D 1 6)) (INT 0)) )
    let testnodVar  = eval g (Expr (ADD (Var "b") (INT 0)) )
    -- ~ let test7 = eval g (ACCUM (Let "a" (D 1 6)) (Expr (BOOL False)))
    -- ~ let test7 = eval g (ACCUM (Let "a" (D 1 6)) (Expr (Gt (MAX (Var "a")) (INT 3))))
    let testgreater = eval g (ACCUM (Let "a" (D 1 6)) (Expr (Gt (MAX (Var "a")) (INT 3))))
    let test8 = eval g (REPUNT (Let "a" (D 1 6)) (Expr (Eq (MAX (Var "a")) (INT 6))))
    
        
    -- ~ print test3
    -- ~ print typetest3
    -- ~ print "test 4: "
    -- ~ print test4
    -- ~ print "test 5: "
    -- ~ print test5
    -- ~ print "test 6: "
    -- ~ print test6

    print "test greater"
    print testgreater
    print "test 8: "
    print test8



    -- ~ print testdivZero 
    -- ~ print testmodZero 
    -- ~ print testnodVar  
