{-# LANGUAGE GADTs #-}

module AST where

import Prelude

-- Collections
type Collection = [Int]

-- Variable identifiers
type Variable = String

-- Possible values that a command can return
type Value = Either Collection Int

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
data Rolls = D Int Int
           | Z Int Int
 deriving Show


-- FilOp representa operadores de filter. (Add boolean)
data FilOp = Grtth Int 
           | Lowth Int 
           | GrtEqt Int 
           | LowEqt Int
           | Equal Int 
           | NEqual Int 
 deriving Show



-- Boolean Expressions

data BoolExp = BOOL Bool
            | Eq (Expression Int) (Expression Int)
            | NEq (Expression Int) (Expression Int)
            | Lt (Expression Int) (Expression Int)
            | Gt (Expression Int) (Expression Int)
            | GEt (Expression Int) (Expression Int)
            | LEt (Expression Int) (Expression Int)
            | IsEmpty (Expression Collection)
            | AND BoolExp BoolExp
            | OR BoolExp BoolExp
            | NOT BoolExp 
            
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
     INDEP  :: Expression Int -> Expression Collection -> Expression Collection


-- Commands
data Command a where
    Expr       :: Expression a -> Command a
    Let        :: Variable -> Expression a -> Command a
    Seq        :: Command a -> Command b -> Command b 
    IfThenElse :: BoolExp -> Command a -> Command a -> Command a 
    ACCUM      :: Command Collection -> Command Collection -> Command Collection    -- Accumulate e1 until e2(is empty)

             -- ~ | REPUNT Command Command -- Repeat Com1 until BoolExp



