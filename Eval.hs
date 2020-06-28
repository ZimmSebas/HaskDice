module Eval where

import DEFS
import FunctionsTest
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
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


eval :: StdGen -> DiceRoll -> ([Int],StdGen)
eval g dice = runRS (do {d1 <- evaldice dice ; d2 <- evaldice dice; return (d1 @@ d2)}) g 

evaldice :: (MonadState m) => DiceRoll -> m [Int]
evaldice (D k n) = do
    g' <- getStd
    let rolls = take k (randomRs (1 :: Int,n) g')
    return rolls
evaldice (Z k n) = do
    g' <- getStd
    let rolls = take k (randomRs (0 :: Int,n) g')
    return rolls
    


-- ~ diceroll :: StdGen -> Dice -> [Int]
-- ~ diceroll g (D k n) = take k (randomRs (0 :: Int,n) g)

mainEval = do  
    g <- newStdGen
    let res = eval g (D 4 6)
    print res
