module Eval where

import DEFS
import Functions
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
    getStd :: StdGen -> m StdGen
    
instance MonadState RandomState where
    getStd sg = RS (\sg -> let (sg1,sg2) = split sg in
                           (sg1,sg2))


-- ~ main = do
    -- ~ g <- newStdGen
    

