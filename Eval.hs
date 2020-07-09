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
initState = [("v",(Left [1,3]))]



---------------------------------------
----- Evaluator -----------------------
---------------------------------------


-- eval is the first function to be called, to eval the result that the main call upon.
eval :: StdGen -> Expression a -> Maybe (a,Env)
eval gen exp = case (runRS (do {res <- evalExp exp; return res}) initState gen) of
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
evalExp :: (MonadState m, MonadError m, MonadRandom m) => Expression a -> m a
evalExp (I n) = return n 
evalExp (C c) = return c 
evalExp (Var v) = lookfor v
evalExp (Roll r) = do
           rolls <- evalRoll r 
           return rolls
evalExp (Least k ce) = do
           rolls <- evalExp ce
           let rolls' = take k (sort rolls)
           return rolls'
evalExp (Largt k ce) = do
           rolls <- evalExp ce
           let rolls' = take k (sortBy (flip compare) rolls)
           return rolls'
evalExp (Filter fop ce) = do
           rolls <- evalExp ce
           let funcfilt = evalFiltOp fop
           return (filter funcfilt rolls)
evalExp (Concat exp1 exp2) = do
           c1 <- evalExp exp1
           c2 <- evalExp exp2
           return (c1 ++ c2)
evalExp (MAX ce) = do
            rolls <- evalExp ce
            return $ foldr max 0 rolls
evalExp (MIN ce) = do
            rolls <- evalExp ce
            return $ foldr min (minBound::Int) rolls
evalExp (SUM ce) = do
            rolls <- evalExp ce
            return $ sum rolls
evalExp (COUNT ce) = do
            rolls <- evalExp ce
            return $ length rolls
evalExp (ADD x y) = do
            x' <- evalExp x
            y' <- evalExp y
            return (x' + y')
evalExp (MINUS x y) = do
            x' <- evalExp x
            y' <- evalExp y
            return (x' - y')
evalExp (TIMES x y) = do
            x' <- evalExp x
            y' <- evalExp y
            return (x' * y')
evalExp (DIV x y) = do
            x' <- evalExp x
            y' <- evalExp y
            if (y' == 0) then throw
                         else return (x' `div` y')
evalExp (MOD x y) = do
            x' <- evalExp x
            y' <- evalExp y
            if (y' == 0) then throw
                         else return (x' `mod` y')
evalExp (UMINUS x) = do
            n  <- evalExp x
            return (n*(-1))
evalExp (SGN x) = do
            n <- evalExp x
            return (signum n)
evalExp (INDEP n c) = do
            cant <- evalExp n
            coll <- evalExp c
            if (cant > 0) then do {coll2 <- evalExp (INDEP (I (cant-1)) c) ; return (coll ++ coll2)}
                          else return coll


-- evalBoolExp takes a Boolean expression and evaluates the result
evalBoolExp :: (MonadState m, MonadError m, MonadRandom m) => BoolExp -> m Bool
evalBoolExp (BOOL b) = return b
evalBoolExp (IsEmpty c) = do
        coll <- evalExp c
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

evalCommand (Expr exp) = undefined
evalCommand (Seq c1 c2) = do
            n <- evalCommand c1
            m <- evalCommand c2
            return m
evalCommand (IfThenElse b c1 c2) = do
            bool <- evalBoolExp b
            if (bool) then (do {res <- evalCommand c1; return res})
                          else (do {res <- evalCommand c2; return res})
-- ~ evalCommand (ACCUM col1 col2) = do
            -- ~ Left c1 <- evalCommand col1
            -- ~ Left c2 <- evalCommand col2
            -- ~ tail <- evalCommand (IfThenElse (IsEmpty (C c2)) (Expr (C c1)) (ACCUM (Expr (C c1)) (Expr (C c2))))
            -- ~ case tail of
                -- ~ Left coll -> do {head <- evalExp (Concat (C c1) (C coll)); return (Left head)}
                -- ~ Right num -> throw
-- ~ evalCommand (Let name e) = do 
            -- ~ let t = typingValue e 
            -- ~ res <- evalExp e
            -- ~ case t of
                -- ~ TColl -> do {update name (Left res) ; return (Left res)}
                -- ~ TInt -> do {update name (Right res) ; return (Right res)}




-- El let y el Expr van a ser un viaje

main = do  
    g <- newStdGen
    let res = eval g (Filter (GrtEqt 3) (Largt 3 (Roll (D 5 8))) )
    let test = eval g (ADD (I 2) (I 4))
    -- ~ let res2 = eval g (Right (UMINUS (MAX (Var "v"))))
    case res of
        Nothing -> print "Buuuh"
        Just (n, st) -> print n
    print res
    print test
