module DEFS where

import Prelude
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

-- Colecciones
-- ~ type Collection = MultiSet (Int,Double)

-- Colecciones
type Collection = [Int]

-- Rolls representa tiradas de dados. 
-- D representa K tiradas de dados de N caras comenzando en 1 (kdn)
-- Z representa K tiradas de de dados de N caras comenzando en 0 (kZn)
-- C representa una tirada previamente evaluada (o sea, una colección)
--                   K   N
data Rolls = D Int Int
           | Z Int Int
           | C Collection
 deriving Show

data FilOp = Gt Int 
           | Lt Int 
           | GEt Int 
           | LEt Int
           | Eq Int 
           | NEq Int 
 deriving Show


-- Operadores de colecciones que generan colecciones
data CollExp = Roll Rolls
             | Least Int CollExp
             | Largt Int CollExp
             | Filter FilOp CollExp -- Filter can be a problem, because is not a "bool"
             | Concat CollExp CollExp
 deriving Show

-- Expresiones sobre colecciones que generan enteros, o enteros
data NumExp = CONST Int
            | MAX CollExp
            | MIN CollExp
            | COLL CollExp
            | SUM CollExp
            | COUNT CollExp
 deriving Show

-- Commands
data Command = Skip
             | Seq Command Command
             | Indep NumExp NumExp
 deriving Show
             -- ~ | IfThenElse Coll Command Command
             -- ~ | REPUNT Command Command
             -- ~ | ACC Command Command 



-- Not sure about this 2 sections ---------

-- Operadores sobre enteros
data BinOp = ADD NumExp NumExp
           | MINUS NumExp NumExp
           | PROD NumExp NumExp
           | DIV NumExp NumExp
           | SGN NumExp
           | MOD NumExp

-- Operadores booleanos

data BoolOp = BOOL Bool
            | AND BoolOp BoolOp
            | OR BoolOp BoolOp
            | IMP BoolOp BoolOp
            

