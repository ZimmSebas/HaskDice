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

-- Operadores de colecciones que generan colecciones
data Coll = Roll Rolls
            | MAX Rolls
            | MIN Rolls
            | Least Int Rolls
            | Largt Int Rolls
            | Filter (Int -> Bool) Rolls
            | Concat Rolls Rolls

-- Expresiones sobre colecciones que generan enteros, o enteros
data NumExp = CONST Int
            | COLL Coll
            | SUM Coll
            | COUNT Coll

-- Commands
data Command = Skip
             | Seq Command Command
             | Indep NumExp NumExp
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
            

