{-# LANGUAGE GADTs #-}

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
----- Initial values ------------------
---------------------------------------

-- Initial State (Null)
initState :: Env
initState = [("v",(C [1,3]))]

---------------------------------------
----- Evaluator -----------------------
---------------------------------------


-- eval is the first function to be called, to eval the result that the main call upon.
eval :: StdGen -> Command a -> Maybe (Value,Env)
eval gen exp = case (runRS (do {res <- evalCommand exp; return res}) initState gen) of
    Nothing            -> Nothing
    Just (val, st, sg) -> Just (val,st)

-- evalRoll takes a Roll and generates a list of dice rolls (Ints), by using pseudo-random number generators.
-- alternative, if it gets a Collection already evaluated, it just returns the collection.
evalRoll :: (MonadState m, MonadError m, MonadRandom m) => Rolls -> m Collection
evalRoll (D k n) = do
    g' <- getStd
    let rolls = take k (randomRs (1 :: Int,n) g')
    return rolls
evalRoll (Z k n) = do
    g' <- getStd
    let rolls = take k (randomRs (0 :: Int,n) g')
    return rolls

-- evalFiltOp eval an operator to the filter, and returns it in a function.

evalFiltOp :: FilOp -> (Int -> Bool)
evalFiltOp (Grtth n)  = (>n)
evalFiltOp (Lowth n)  = (<n)
evalFiltOp (GrtEqt n) = (>=n)
evalFiltOp (LowEqt n) = (<=n)
evalFiltOp (Equal n)  = (==n)
evalFiltOp (NEqual n) = (/=n)

-- evalExp takes any kind of expression and returns the representation.
evalExp :: (MonadState m, MonadError m, MonadRandom m) => Expression a -> m Value
evalExp (INT n) = return (I n) 
evalExp (COLL c) = return (C c) 
evalExp (Var v) = lookfor v
evalExp (Roll r) = do
           rolls <- evalRoll r 
           return (C rolls)
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
            return (I $ foldr min (minBound::Int) rolls)
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
            if (y' == 0) then throw
                         else return $ I (x' `div` y')
evalExp (MOD x y) = do
            (I x') <- evalExp x
            (I y') <- evalExp y
            if (y' == 0) then throw
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


-- evalBoolExp takes a Boolean expression and evaluates the result
evalBoolExp :: (MonadState m, MonadError m, MonadRandom m) => BoolExp -> m Bool
evalBoolExp (BOOL b) = return b
evalBoolExp (IsEmpty c) = do
        (C coll) <- evalExp c
        return (coll == [])
evalBoolExp (Eq e1 e2) = do
        exp1 <- evalExp e1
        exp2 <- evalExp e2
        return (exp1 == exp2)
evalBoolExp (NEq e1 e2) = do
        exp1 <- evalExp e1
        exp2 <- evalExp e2
        return (exp1 /= exp2)
evalBoolExp (Lt e1 e2) = do
        exp1 <- evalExp e1
        exp2 <- evalExp e2
        return (exp1 < exp2)
evalBoolExp (Gt e1 e2) = do
        exp1 <- evalExp e1
        exp2 <- evalExp e2
        return (exp1 > exp2)
evalBoolExp (LEt e1 e2) = do
        exp1 <- evalExp e1
        exp2 <- evalExp e2
        return (exp1 <= exp2)
evalBoolExp (GEt e1 e2) = do
        exp1 <- evalExp e1
        exp2 <- evalExp e2
        return (exp1 <= exp2)
evalBoolExp (AND b1 b2) = do
        x <- evalBoolExp b1
        y <- evalBoolExp b2
        return (x && y)
evalBoolExp (OR b1 b2) = do
        x <- evalBoolExp b1
        y <- evalBoolExp b2
        return (x || y)
evalBoolExp (NOT b) = do
        x <- evalBoolExp b
        return (not x)

-- eval Command takes a command and evaluates the changes in the state.
-- Eval Command returns a Value (Either Collection Int), based on what i had evalued.
-- La cosa es que el eval de commands va a devolver un Value. Entonces devuelve todo junto y que haya un comando Print que printee y listo. Ces't fini.
 
evalCommand :: (MonadState m, MonadError m, MonadRandom m) => Command a -> m Value
evalCommand (Expr exp) = do
            e <- evalExp exp
            return e
evalCommand (Seq c1 c2) = do
            n <- evalCommand c1
            m <- evalCommand c2
            return m
evalCommand (IfThenElse b c1 c2) = do
            bool <- evalBoolExp b
            if (bool) then (do {res <- evalCommand c1; return res})
                          else (do {res <- evalCommand c2; return res})
evalCommand (Let name e) = do  
            res <- evalExp e
            update name res
            return res
evalCommand (REPUNT (Let v c) col2) = do
            reslet <- evalCommand (Let v c)
            case reslet of
                (C e1) -> do {(C c2) <- evalCommand col2;
                              res <- evalCommand (IfThenElse (IsEmpty (COLL c2)) (Expr (COLL e1)) (REPUNT (Let v c) (Expr (COLL c2))));
                              return res}
                (I numb) -> throw
evalCommand (ACCUM (Let v c) col2) = do
            reslet <- evalCommand (Let v c)
            case reslet of
                (C head) -> do {(C c2) <- evalCommand col2;
                                res <- evalCommand (IfThenElse (IsEmpty (COLL c2)) (Expr (COLL head)) (ACCUM (Expr (COLL head)) (Expr (COLL c2))));
                                case res of
                                    (C tail) -> do {(C list) <- evalExp (Concat (COLL head) (COLL tail)); return (C list)}
                                    (I numb) -> throw ;}
                (I numb) -> throw

-- Voy a tener que implementar booleanos con los REPUNT y ACCUM. Ver cómo hacer eso.


main = do  
    g <- newStdGen
    let res = eval g (Expr (Filter (GrtEqt 3) (Largt 3 (Roll (D 5 8))) ))
    let test1 = eval g (Let "x" (COLL [1,2,3]))
    case res of
        Nothing -> print "Buuuh"
        Just (n, st) -> print n
    print test1
    -- ~ print test2
    -- ~ print test3
