module RandomState where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Data.List
import System.IO
import System.Random 
import Prelude

-- Variable State
type Env = [(Variable,Value)]


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
    lookfor :: Variable -> m Value
    -- Updates a variable value
    update :: Variable -> Value -> m ()

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


-- Class that represent monads that works with randomness
class Monad m => MonadRandom m where
    -- getStd generates a split from the generator to maintain randomness.
    getStd :: m StdGen
    
instance MonadRandom RandomState where
    getStd = RS (\st sg -> let (sg1,sg2) = split sg in
                           Just (sg1,st,sg2))
