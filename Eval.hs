module Eval where

import DEFS
import Functions
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Data.List
import System.IO
import System.Random 
import Prelude


newtype RandomState a = RS {runRS :: StdGen -> (a, StdGen)} 

-- Esto está mal, el objetivo es que realice un pasaje con split sobre el sg. Que pase un sg y devuelva otro sg.
-- Not really sure if this works :

instance Monad RandomState where 
    return x = RS (\sg -> (x, sg))
    m >>= f  = RS (\sg -> let (d, sg')  = runRS m sg in
                            runRS (f d) sg')

instance Functor RandomState where
    fmap = liftM
 
instance Applicative RandomState where
    pure   = return
    (<*>)  = ap      

class Monad m => MonadState m where
    getStd :: m StdGen
    
instance MonadState RandomState where
    getStd = RS (\sg -> let (sg1,sg2) = split sg in
                           (sg1,sg2))


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

-- EvalFiltOp evalua un operador posible para filter y lo devuelve en forma de función
evalFiltOp :: FilOp -> (Int -> Bool)
evalFiltOp (Gt n)  = (>n)
evalFiltOp (Lt n)  = (<n)
evalFiltOp (GEt n) = (>=n)
evalFiltOp (LEt n) = (<=n)
evalFiltOp (Eq n)  = (==n)
evalFiltOp (NEq n) = (/=n)


-- evalColl toma una expresión de colección y devuelve la evaluación de este.
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
            
            
mainEval = do  
    g <- newStdGen
    let res = eval g (Filter (Lt 3) (Largt 3 (Roll (D 10 3))))
    print res
