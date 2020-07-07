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
           | C Collection
 deriving Show


-- FilOp representa operadores de filter.
data FilOp = Gt Int 
           | Lt Int 
           | GEt Int 
           | LEt Int
           | Eq Int 
           | NEq Int 
 deriving Show


-- CollExp representa operadores de colecciones que generan colecciones:
-- Roll es un constructor sobre el tipo de datos Rolls
-- Least y Largt representan los N elementos más chicos/grandes (respectivamente).
-- Filter es la función filter con las operaciones permitidas.
-- Concat concatena dos resultados.
data CollExp = Roll Rolls
             | Var Variable
             | Least Int CollExp
             | Largt Int CollExp
             | Filter FilOp CollExp -- Filter can be a problem, should i get a boolean exp?
             | Concat CollExp CollExp
 deriving Show

-- NumExp representa expresiones sobre colecciones o enteros y que generan enteros.
-- Const es una constante
-- Max y min calculan el mínimo de una colección
-- Sum y count suman los resultados de una colección o los cuentan, respectivamente.
data NumExp = CONST Int
            | MAX CollExp
            | MIN CollExp
            | SUM CollExp
            | COUNT CollExp
            | ADD NumExp NumExp
            | MINUS NumExp NumExp
            | TIMES NumExp NumExp
            | DIV NumExp NumExp
            | MOD NumExp NumExp
            | UMINUS NumExp
            | SGN NumExp
 deriving Show
 
-- Expr representa expresiones que pueden ser tanto CollExp como NumExp. Necesario para evaluar.
type Expr = Either CollExp NumExp

-- Commands
data Command = Skip
             | Single Expr
             | Let Variable CollExp
             | Seq Command Command
             | IfThenElse CollExp Command Command
 deriving Show

             -- ~ | Indep NumExp CollExp
             -- ~ | Print Expr
             -- ~ | REPUNT Command Command
             -- ~ | ACC Command Command 

-- GADT


-- Not sure about this section ---------

-- Boolean Expressions

data BoolOp = BOOL Bool
            | AND BoolOp BoolOp
            | OR BoolOp BoolOp
            | IMP BoolOp BoolOp
            

