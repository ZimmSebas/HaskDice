{-# LANGUAGE GADTs #-}

module AST where

import Prelude
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

-- Collections
-- ~ type Collection = MultiSet (Int,Double)

-- Collections
type Collection = [Int]

-- Variable identifiers
type Variable = String

-- Possible values that a command can return
type Value = Either Collection Int


-- DKN representa ...
-- Rolls representa tiradas de dados. 
-- D representa K tiradas de dados de N caras comenzando en 1 (kdn)
-- Z representa K tiradas de de dados de N caras comenzando en 0 (kZn)
-- C representa una tirada previamente evaluada (o sea, una colección)
--              K   N
data Rolls = D Int Int
           | Z Int Int
 deriving Show


-- FilOp representa operadores de filter.
data FilOp = Gt Int 
           | Lt Int 
           | GEt Int 
           | LEt Int
           | Eq Int 
           | NEq Int 
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
     Roll   :: Rolls -> Expression Collection
     I      :: Int -> Expression Int
     C      :: Collection -> Expression Collection
     Var    :: Variable -> Expression Value
     Least  :: Int -> Expression Collection -> Expression Collection
     Largt  :: Int -> Expression Collection -> Expression Collection
     Filter :: FilOp -> Expression Collection -> Expression Collection
     Concat :: Expression Collection -> Expression Collection -> Expression Collection
     MAX    :: Expression Collection -> Expression Int
     MIN    :: Expression Collection -> Expression Int
     SUM    :: Expression Collection -> Expression Int
     COUNT  :: Expression Collection -> Expression Int
     ADD    :: Expression Int -> Expression Int -> Expression Int
     MINUS  :: Expression Int -> Expression Int -> Expression Int
     TIMES  :: Expression Int -> Expression Int -> Expression Int
     DIV    :: Expression Int -> Expression Int -> Expression Int
     MOD    :: Expression Int -> Expression Int -> Expression Int
     UMINUS :: Expression Int -> Expression Int 
     SGN    :: Expression Int -> Expression Int 


-- Commands
data Command a where
    Val        :: Command Value
    Expr       :: Expression a -> Command Value
    Let        :: Variable -> Expression a -> Command Value 
    Seq        :: Command a -> Command b -> Command Value 
    IfThenElse :: (Expression Collection) -> Command a -> Command b -> Command Value 

             -- ~ | Indep NumExp CollExp
             -- ~ | Print Expr
             -- ~ | REPUNT Command Command
             -- ~ | ACC Command Command 


-- Not sure about this section ---------

-- Boolean Expressions

data BoolOp = BOOL Bool
            | AND BoolOp BoolOp
            | OR BoolOp BoolOp
            | IMP BoolOp BoolOp
            

