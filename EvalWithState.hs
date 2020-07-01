module Eval where

import DEFS
import Functions
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Data.List
import System.IO
import System.Random 
import Prelude


---------------------------------------
----- Initial values and types --------
---------------------------------------

-- Variable State
type Env = [(Variable,Collection)]

-- Initial State (Null)
initState :: Env
initState = [("v",[1,3])]


---------------------------------------
----- Monads --------------------------
---------------------------------------

newtype RandomState a = RS { runRS :: Env -> StdGen -> Maybe (a, Env, StdGen) }

instance Functor RandomState where
    fmap = liftM
 
instance Applicative RandomState where
    pure   = return
    (<*>)  = ap      

instance Monad RandomState where 
    return d = RS (\st sg -> Just (d, st, sg))
    m >>= f  = RS (\st sg -> case (runRS m st sg) of
                        Nothing            -> Nothing
                        Just (d, st', sg') -> runRS (f d) st' sg')



-- Class that represents monads with an enviroment of variables
class Monad m => MonadState m where
    -- Search a variable value
    lookfor :: Variable -> m Collection
    -- Updates a variable value
    update :: Variable -> Collection -> m ()

instance MonadState RandomState where
    lookfor v = RS (\st sg -> case (lookfor' v st sg) of
                        Nothing -> Nothing
                        Just j  -> Just (j, st, sg))
            where lookfor' v []          sg = Nothing
                  lookfor' v ((u, j):ss) sg | v == u = Just j
                                            | v /= u = lookfor' v ss sg
    update v val = RS (\st sg -> Just ((), update' v val st, sg))
                   where update' v i []          = [(v, i)]
                         update' v i ((u, _):ss) | v == u = (v, i):ss
                         update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)


-- Class that represent monads that has possible errors
class Monad m => MonadError m where
    -- Throws an error
    throw :: m a

instance MonadError RandomState where
    throw = RS (\st sg -> Nothing)


-- getStd generates a split from the generator to maintain randomness.

class Monad m => MonadRandom m where
    getStd :: m StdGen
    
instance MonadRandom RandomState where
    getStd = RS (\st sg -> let (sg1,sg2) = split sg in
                           Just (sg1,st,sg2))


---------------------------------------
----- Evaluator -----------------------
---------------------------------------


-- eval is the first function to be called, to eval the result that the main call upon.

eval :: StdGen -> NumExp -> Maybe (Int,Env)
eval gen exp = case (runRS (do {res <- evalNumExp exp; return res}) initState gen) of
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
evalRoll (C l) = return l

-- evalFiltOp eval an operator to the filter, and returns it in a function.
evalFiltOp :: FilOp -> (Int -> Bool)
evalFiltOp (Gt n)  = (>n)
evalFiltOp (Lt n)  = (<n)
evalFiltOp (GEt n) = (>=n)
evalFiltOp (LEt n) = (<=n)
evalFiltOp (Eq n)  = (==n)
evalFiltOp (NEq n) = (/=n)


-- evalCollExp takes any kind of collection expresion and returns a collection
evalCollExp :: (MonadState m, MonadError m, MonadRandom m) => CollExp -> m Collection
evalCollExp (Roll r) = do
               rolls <- evalRoll r 
               return rolls
evalCollExp (Var v) = lookfor v
evalCollExp (Least k ce) = do
               rolls <- evalCollExp ce
               let rolls' = take k (sort rolls)
               return rolls'
evalCollExp (Largt k ce) = do
               rolls <- evalCollExp ce
               let rolls' = take k (sortBy (flip compare) rolls)
               return rolls'
evalCollExp (Filter fop ce) = do
               rolls <- evalCollExp ce
               let funcfilt = evalFiltOp fop
               return (filter funcfilt rolls)
evalCollExp (Concat exp1 exp2) = do
               c1 <- evalCollExp exp1
               c2 <- evalCollExp exp2
               return (c1 @@ c2)

-- evalNumExp takes any kind of numerical expression and makes the evaluation, returning the integer.
evalNumExp :: (MonadState m, MonadError m, MonadRandom m) => NumExp -> m Int
evalNumExp (CONST n) = return n
evalNumExp (MAX ce) = do
            rolls <- evalCollExp ce
            return $ foldr max 0 rolls
evalNumExp (MIN ce) = do
            rolls <- evalCollExp ce
            return $ foldr min (minBound::Int) rolls
evalNumExp (SUM ce) = do
            rolls <- evalCollExp ce
            return $ sum rolls
evalNumExp (COUNT ce) = do
            rolls <- evalCollExp ce
            return $ length rolls
evalNumExp (ADD x y) = do
            x' <- evalNumExp x
            y' <- evalNumExp y
            return (x' + y')
evalNumExp (MINUS x y) = do
            x' <- evalNumExp x
            y' <- evalNumExp y
            return (x' - y')
evalNumExp (TIMES x y) = do
            x' <- evalNumExp x
            y' <- evalNumExp y
            return (x' * y')
evalNumExp (DIV x y) = do
            x' <- evalNumExp x
            y' <- evalNumExp y
            if (y' == 0) then throw
                         else return (x' `div` y')
evalNumExp (MOD x y) = do
            x' <- evalNumExp x
            y' <- evalNumExp y
            if (y' == 0) then throw
                         else return (x' `mod` y')
evalNumExp (UMINUS x) = do
            n  <- evalNumExp x
            return (n*(-1))
evalNumExp (SGN x) = do
            n <- evalNumExp x
            return (signum n)

            
-- eval Command takes a command and evaluates the changes in the state.


mainEval = do  
    g <- newStdGen
    let res = eval g (DIV (MAX (Filter (GEt 3) (Largt 3 (Roll (D 5 8))))) (CONST 0))
    let res2 = eval g (UMINUS (MAX (Var "v")))
    case res2 of
        Nothing -> print "Buuuh"
        Just (n, st) -> print n
    print res2

