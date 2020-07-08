{-# LANGUAGE GADTs #-}

module Eval where

import AST
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
type Env = [(Variable,Value)]

-- Initial State (Null)
initState :: Env
initState = [("v",(Left [1,3]))]


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
evalFiltOp (Gt n)  = (>n)
evalFiltOp (Lt n)  = (<n)
evalFiltOp (GEt n) = (>=n)
evalFiltOp (LEt n) = (<=n)
evalFiltOp (Eq n)  = (==n)
evalFiltOp (NEq n) = (/=n)

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


-- eval Command takes a command and evaluates the changes in the state.
-- Eval Command returns a Value (Either Collection Int), based on what i had evalued.
-- La cosa es que el eval de commands va a devolver un Value. Entonces devuelve todo junto y que haya un comando Print que printee y listo. Ces't fini.

-- ~ data Command a where
    -- ~ Expr       :: Expression a -> Command Value
    -- ~ Let        :: Variable -> Expression a -> Command Value 
    -- ~ Seq        :: Command a -> Command b -> Command Value 
    -- ~ IfThenElse :: (Expression Collection) -> Command a -> Command b -> Command Value 

evalCommand :: (MonadState m, MonadError m, MonadRandom m) => Command a -> m Value
evalCommand (Seq c1 c2) = do
            n <- evalCommand c1
            m <- evalCommand c2
            return m
evalCommand (IfThenElse coll c1 c2) = do
            co <- evalExp coll
            if (co == []) then (do {res <- evalCommand c1; return res})
                          else (do {res <- evalCommand c2; return res})
-- ~ evalCommand (Let name e) = do 
            -- ~ res <- evalExp e
            -- ~ update name res
            -- ~ return res

-- El let y el Expr van a ser un viaje

main = do  
    g <- newStdGen
    let res = eval g (Filter (GEt 3) (Largt 3 (Roll (D 5 8))) )
    -- ~ let res2 = eval g (Right (UMINUS (MAX (Var "v"))))
    case res of
        Nothing -> print "Buuuh"
        Just (n, st) -> print n
    print res
