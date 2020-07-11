{-# LANGUAGE GADTs #-}

module AST where

import Prelude

-- Collections
type Collection = [Int]

-- Variable identifiers
type Variable = String

-- Possible values that a command can return
data Value = C Collection 
           | I Int
           | B Bool
 deriving (Show, Eq, Ord)
 
data Error = TypingError Type Type (Expression Value)
           | VarNotExist String 
           | DivByZero (Expression Int) (Expression Int) 
           | ModByZero (Expression Int) (Expression Int)
 -- ~ deriving Show -- Need a instance for show



data Return a = Result a
              | Crash Error 
 -- ~ deriving Show

-- Possible Types for the TypeSystem
data Type = TInt
            | TColl
            | TBool
 deriving(Eq, Ord)
        -- ~ | TFun Type Type


instance Show Type where
    show TInt  = "int"
    show TColl = "collection"
    show TBool = "bool"

-- DKN representa ...
-- Rolls representa tiradas de dados. 
-- D representa K tiradas de dados de N caras comenzando en 1 (kdn)
-- Z representa K tiradas de de dados de N caras comenzando en 0 (kZn)
-- C representa una tirada previamente evaluada (o sea, una colección)
--              K   N
-- ~ data Rolls = D Int Int
           -- ~ | Z Int Int
 -- ~ deriving Show


-- FilOp representa operadores de filter. (Add boolean)
data FilOp = Grtth Int 
           | Lowth Int 
           | GrtEqt Int 
           | LowEqt Int
           | Equal Int 
           | NEqual Int 
 deriving Show



-- Expression representa operadores de colecciones.
-- Roll es un constructor sobre el tipo de datos Rolls
-- Least y Largt representan los N elementos más chicos/grandes (respectivamente).
-- Filter es la función filter con las operaciones permitidas.
-- Concat concatena dos resultados.
-- Const es una constante
-- Max y min calculan el mínimo de una colección
-- Sum y count suman los resultados de una colección o los cuentan, respectivamente.

data Expression a where 
     D       :: Int -> Int -> Expression Collection
     Z       :: Int -> Int -> Expression Collection
     INT     :: Int -> Expression Int
     COLL    :: Collection -> Expression Collection
     Var     :: Variable -> Expression Value
     Least   :: Int -> Expression Collection -> Expression Collection
     Largt   :: Int -> Expression Collection -> Expression Collection
     Filter  :: FilOp -> Expression Collection -> Expression Collection
     Concat  :: Expression a -> Expression a -> Expression Collection
     MAX     :: Expression a -> Expression Int
     MIN     :: Expression a -> Expression Int
     SUM     :: Expression Collection -> Expression Int
     COUNT   :: Expression Collection -> Expression Int
     ADD     :: Expression Int -> Expression Int -> Expression Int
     MINUS   :: Expression Int -> Expression Int -> Expression Int
     TIMES   :: Expression Int -> Expression Int -> Expression Int
     DIV     :: Expression Int -> Expression Int -> Expression Int
     MOD     :: Expression Int -> Expression Int -> Expression Int
     UMINUS  :: Expression Int -> Expression Int 
     SGN     :: Expression Int -> Expression Int
     INDEP   :: Expression Int -> Expression Collection -> Expression Collection
     BOOL    :: Bool -> Expression Bool
     Eq      :: Expression Int -> Expression Int -> Expression Bool
     NEq     :: Expression Int -> Expression Int -> Expression Bool
     Lt      :: Expression Int -> Expression Int -> Expression Bool
     Gt      :: Expression Int -> Expression Int -> Expression Bool
     GEt     :: Expression Int -> Expression Int -> Expression Bool
     LEt     :: Expression Int -> Expression Int -> Expression Bool
     AND     :: Expression Bool -> Expression Bool -> Expression Bool
     OR      :: Expression Bool -> Expression Bool -> Expression Bool
     NOT     :: Expression Bool -> Expression Bool
     IsEmpty :: Value -> Expression Bool
 
instance Show (Expression a) where
    show (D k n) = show k ++ "D" ++ show n 
    show (Z k n) = show k ++ "Z" ++ show n
     


 
-- Commands
data Command a where
    Expr       :: Expression a -> Command a
    Let        :: Variable -> Expression a -> Command a
    Seq        :: Command a -> Command b -> Command b 
    IfThenElse :: Expression Bool -> Command a -> Command b -> Command c 
    ACCUM      :: Command Collection -> Command Collection -> Command Collection    -- Accumulate e1 until e2(is empty)
    REPUNT     :: Command Collection -> Command Collection -> Command Collection



