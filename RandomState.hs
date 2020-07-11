{-# LANGUAGE MultiParamTypeClasses #-}

module RandomState where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Data.List
import System.IO
import System.Random 
import Prelude

---------------------------------------
----- Initial types and values --------
---------------------------------------

-- Initial State (Null)
initState :: Env
initState = [("v",(C [1,3]))]

-- Variable State
type Env = [(Variable,Value)]


initStateType ::TypEnv
initStateType = [("v",TColl)]

-- Typing State of variable (for type eval)
type TypEnv = [(Variable, Type)]

---------------------------------------
----- Random State Monad --------------
---------------------------------------

newtype RandomState a = RS { runRS :: Env -> StdGen -> Result (a, Env, StdGen) }

instance Functor RandomState where
    fmap = liftM
 
instance Applicative RandomState where
    pure   = return
    (<*>)  = ap      

instance Monad RandomState where 
    return d = RS (\st sg -> Return (d, st, sg))
    m >>= f  = RS (\st sg -> case (runRS m st sg) of
                        Crash e              -> Crash e
                        Return (d, st', sg') -> runRS (f d) st' sg')


---------------------------------------
----- Type State Monad ----------------
---------------------------------------

newtype TypeState a = TS { runTS :: TypEnv -> Result (a,TypEnv) }

instance Functor TypeState where
    fmap = liftM
 
instance Applicative TypeState where
    pure   = return
    (<*>)  = ap      

instance Monad TypeState where 
    return t = TS (\st -> Return (t, st))
    m >>= f  = TS (\st -> case (runTS m st) of
                        Crash e         -> Crash e
                        Return (t, st') -> runTS (f t) st')


---------------------------------------
----- Monad Classes and Instances -----
---------------------------------------



-- Class that represents monads with an enviroment of variables
class Monad m => MonadState m a where
    -- Search a variable value
    lookfor :: Variable -> m a
    -- Updates a variable value
    update :: Variable -> a -> m ()

instance MonadState RandomState Value where
    lookfor v = RS (\st sg -> case (lookfor' v st sg) of
                        Crash (VarNotExist v) -> Crash (VarNotExist v)
                        Return j              -> Return (j, st, sg))
            where lookfor' v []          sg = Crash (VarNotExist v)
                  lookfor' v ((u, j):ss) sg | v == u = Return j
                                            | v /= u = lookfor' v ss sg
    update v val = RS (\st sg -> Return ((), update' v val st, sg))
                   where update' v i []          = [(v, i)]
                         update' v i ((u, _):ss) | v == u = (v, i):ss
                         update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)

instance MonadState TypeState Type where
    lookfor v = TS (\st -> case (lookfor' v st) of
                        Crash (VarNotExist v) -> Crash (VarNotExist v)
                        Return j              -> Return (j, st))
            where lookfor' v []          = Crash (VarNotExist v)
                  lookfor' v ((u, j):ss) | v == u = Return j
                                         | v /= u = lookfor' v ss
    update v val = TS (\st -> Return ((), update' v val st))
                   where update' v i []          = [(v, i)]
                         update' v i ((u, _):ss) | v == u = (v, i):ss
                         update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)



-- Class that represent monads that has possible errors
class Monad m => MonadError m where
    throwTypingError  :: Type -> Type -> String -> m a -- The Value thing may change
    throwVarNotExist  :: String -> m a
    throwDivByZero    :: Expression Int -> Expression Int -> m a
    throwModByZero    :: Expression Int -> Expression Int -> m a

instance MonadError RandomState where
    throwTypingError t1 t2 s = RS (\st sg -> Crash $ TypingError t1 t2 s)
    throwVarNotExist v       = RS (\st sg -> Crash $ VarNotExist v)
    throwDivByZero e1 e2     = RS (\st sg -> Crash $ DivByZero e1 e2)
    throwModByZero e1 e2     = RS (\st sg -> Crash $ ModByZero e1 e2)

    -- ~ throwTypingError t1 t2 s = RS (\st sg -> Nothing)
    -- ~ throwVarNotExist v       = RS (\st sg -> Nothing)
    -- ~ throwDivByZero e1 e2     = RS (\st sg -> Nothing)
    -- ~ throwModByZero e1 e2     = RS (\st sg -> Nothing)

instance MonadError TypeState where
    throwTypingError t1 t2 s = TS (\st -> Crash $ TypingError t1 t2 s)
    throwVarNotExist v       = TS (\st -> Crash $ VarNotExist v)
    throwDivByZero e1 e2     = TS (\st -> Crash $ DivByZero e1 e2)
    throwModByZero e1 e2     = TS (\st -> Crash $ ModByZero e1 e2)

-- Class that represent monads that works with randomness
class Monad m => MonadRandom m where
    -- getStd generates a split from the generator to maintain randomness.
    getStd :: m StdGen
    
instance MonadRandom RandomState where
    getStd = RS (\st sg -> let (sg1,sg2) = split sg in
                           Return (sg1,st,sg2))
