module Eval where

import DEFS
import Functions
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Data.List
import System.IO
import System.Random 
import Prelude


-- RandomState monad, keeps the StdGen (random number generator) as the impure value.

newtype RandomState a = RS {runRS :: StdGen -> (a, StdGen)} 

instance Monad RandomState where 
    return x = RS (\sg -> (x, sg))
    m >>= f  = RS (\sg -> let (d, sg')  = runRS m sg in
                            runRS (f d) sg')

instance Functor RandomState where
    fmap = liftM
 
instance Applicative RandomState where
    pure   = return
    (<*>)  = ap      



-- getStd generates a split from the generator to maintain randomness.

class Monad m => MonadState m where
    getStd :: m StdGen
    
instance MonadState RandomState where
    getStd = RS (\sg -> let (sg1,sg2) = split sg in
                           (sg1,sg2))


-- eval is the first function to be called, to eval the result that the main call upon.

eval :: StdGen -> CollExp -> ([Int],StdGen)
eval g exp = runRS (do {res <- evalColl exp; return res}) g 


-- evalRoll takes a Roll and generates a list of dice rolls (Ints), by using pseudo-random number generators.
-- alternative, if it gets a Collection already evaluated, it just returns the collection.
evalRoll :: (MonadState m) => Rolls -> m Collection
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


-- evalColl takes any kind of collection expresion and returns a collection
evalColl :: (MonadState m) => CollExp -> m Collection
evalColl (Roll r) = do
            rolls <- evalRoll r 
            return rolls
evalColl (Least k ce) = do
            rolls <- evalColl ce
            let rolls' = take k (sort rolls)
            return rolls'
evalColl (Largt k ce) = do
            rolls <- evalColl ce
            let rolls' = take k (sortBy (flip compare) rolls)
            return rolls'
evalColl (Filter fop ce) = do
            rolls <- evalColl ce
            let funcfilt = evalFiltOp fop
            return (filter funcfilt rolls)
evalColl (Concat exp1 exp2) = do
            c1 <- evalColl exp1
            c2 <- evalColl exp2
            return (c1 @@ c2)



-- evalNumExp takes any kind of numerical expression and makes the evaluation, returning the integer.
evalNumExp :: (MonadState m) => NumExp -> m Int
evalNumExp (CONST n) = return n
evalNumExp (MAX ce) = do
            rolls <- evalColl ce
            return (foldr max 0 rolls)
evalNumExp (MIN ce) = do
            rolls <- evalColl ce
            return (foldr min (minBound::Int) rolls)
evalNumExp (SUM ce) = do
            rolls <- evalColl ce
            return (sum rolls)
evalNumExp (COUNT ce) = do
            rolls <- evalColl ce
            return (length rolls)


-- eval Command se va a enfrentar al tema de que necesitaría que pueda retornar algo decente.


mainEval = do  
    g <- newStdGen
    let res = fst $ eval g (Filter (GEt 5) (Largt 3 (Roll (D 4 6))))
    print res
