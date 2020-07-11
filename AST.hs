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
 deriving (Eq, Ord)
 
instance Show Value where
    show (C c) = show c
    show (I i) = show i
    show (B b) = show b
 
data Error = TypingError Type Type String
           | VarNotExist String 
           | DivByZero String String 
           | ModByZero String String

instance Show Error where
    show (TypingError t1 t2 e) = "\n -- Error: Type error -- \nExpecting type: " ++
        show t1 ++
        " \nbut found type: " ++
        show t2 ++
        " \nin expression " ++
        show e ++ ".\n"
    show (VarNotExist s) = "\n -- Error: Variable does not exist -- \nThe variable \"" ++ id s ++ "\" does not exist"
    show (DivByZero e1 e2) = "\n -- Error: Division by Zero -- \n" ++ 
        "A division by zero ocurred when dividing: \n" ++
        show e1 ++
        " \nby: \n" ++
        show e2 ++ ".\n"
    show (ModByZero e1 e2) = "\n -- Error: Mod by Zero -- \n" ++ 
        "A mod by zero ocurred when calculating mod of\n" ++
        show e1 ++
        " \nby: \n" ++
        show e2 ++ ".\n"


data Result a = Return a
              | Crash Error

instance Show a => Show (Result a) where
    show (Return r) = show r
    show (Crash e)  = show e

-- Possible Types for the TypeSystem
data Type = TInt
            | TColl
            | TBool
 deriving(Eq, Ord)
        -- ~ | TFun Type Type


instance Show Type where
    show TInt  = "Int"
    show TColl = "Collection"
    show TBool = "Bool"




-- FilOp representa operadores de filter. (Add boolean)
data FilOp = Grtth Int 
           | Lowth Int 
           | GrtEqt Int 
           | LowEqt Int
           | Equal Int 
           | NEqual Int 

instance Show FilOp where
    show (Grtth i)  = "(>" ++ show i ++ ")" 
    show (Lowth i)  = "(<" ++ show i ++ ")" 
    show (GrtEqt i) = "(>=" ++ show i ++ ")" 
    show (LowEqt i) = "(<=" ++ show i ++ ")" 
    show (Equal i)  = "(==" ++ show i ++ ")" 
    show (NEqual i) = "(/=" ++ show i ++ ")" 



-- Expression representa operadores de colecciones.
-- Roll es un constructor sobre el tipo de datos Rolls
-- Least y Largt representan los N elementos más chicos/grandes (respectivamente).
-- Filter es la función filter con las operaciones permitidas.
-- Concat concatena dos resultados.
-- Const es una constante
-- Max y min calculan el mínimo de una colección
-- Sum y count suman los resultados de una colección o los cuentan, respectivamente.

-- DKN representa ...
-- Rolls representa tiradas de dados. 
-- D representa K tiradas de dados de N caras comenzando en 1 (kdn)
-- Z representa K tiradas de de dados de N caras comenzando en 0 (kZn)
-- C representa una tirada previamente evaluada (o sea, una colección)
--              K   N

data Expression a where 
     D       :: Int -> Int -> Expression Collection
     Z       :: Int -> Int -> Expression Collection
     INT     :: Int -> Expression Int
     COLL    :: Collection -> Expression Collection
     Var     :: Variable -> Expression Value
     Least   :: Int -> Expression a -> Expression Collection
     Largt   :: Int -> Expression a -> Expression Collection
     Filter  :: FilOp -> Expression a -> Expression Collection
     Concat  :: Expression a -> Expression b -> Expression Collection
     MAX     :: Expression a -> Expression Int
     MIN     :: Expression a -> Expression Int
     SUM     :: Expression a -> Expression Int
     COUNT   :: Expression a -> Expression Int
     ADD     :: Expression a -> Expression b -> Expression Int
     MINUS   :: Expression a -> Expression b -> Expression Int
     TIMES   :: Expression a -> Expression b -> Expression Int
     DIV     :: Expression a -> Expression b -> Expression Int
     MOD     :: Expression a -> Expression b -> Expression Int
     UMINUS  :: Expression a -> Expression Int 
     SGN     :: Expression a -> Expression Int
     INDEP   :: Expression a -> Expression b -> Expression Collection
     BOOL    :: Bool -> Expression Bool
     Eq      :: Expression a -> Expression b -> Expression Bool
     NEq     :: Expression a -> Expression b -> Expression Bool
     Lt      :: Expression a -> Expression b -> Expression Bool
     Gt      :: Expression a -> Expression b -> Expression Bool
     GEt     :: Expression a -> Expression b -> Expression Bool
     LEt     :: Expression a -> Expression b -> Expression Bool
     AND     :: Expression a -> Expression b -> Expression Bool
     OR      :: Expression a -> Expression b -> Expression Bool
     NOT     :: Expression a -> Expression Bool
     IsEmpty :: Value -> Expression Bool
 
instance Show (Expression a) where
    show (D k n)        = show k ++ "D" ++ show n 
    show (Z k n)        = show k ++ "Z" ++ show n
    show (INT i)        = show i
    show (COLL c)       = show c
    show (Var v)        = id v
    show (Least i c)    = "least " ++ show i ++ " " ++ show c
    show (Largt i c)    = "largest " ++ show i ++ " " ++ show c
    show (Filter fop c) = "filter " ++ show fop ++ " " ++ show c
    show (Concat c1 c2) = show c1 ++ " ++ " ++ show c2
    show (MAX e)        = "max " ++ show e 
    show (MIN e)        = "min " ++ show e 
    show (SUM e)        = "sum " ++ show e 
    show (COUNT e)      = "count " ++ show e 
    show (ADD e1 e2)    = show e1 ++ " + " ++ show e2
    show (MINUS e1 e2)  = show e1 ++ " - " ++ show e2
    show (TIMES e1 e2)  = show e1 ++ " * " ++ show e2
    show (DIV e1 e2)    = show e1 ++ " / " ++ show e2
    show (MOD e1 e2)    = show e1 ++ " % " ++ show e2
    show (UMINUS e1)    = " -" ++ show e1 
    show (SGN e1)       = "sgn " ++ show e1 
    show (INDEP n c)    = show n ++ " # " ++ show c 
    show (BOOL b)       = show b
    show (Eq e1 e2)     = show e1 ++ " == " ++ show e2
    show (NEq e1 e2)    = show e1 ++ " /= " ++ show e2
    show (Lt e1 e2)     = show e1 ++ " < " ++ show e2
    show (Gt e1 e2)     = show e1 ++ " > " ++ show e2
    show (GEt e1 e2)    = show e1 ++ " >= " ++ show e2
    show (LEt e1 e2)    = show e1 ++ " <= " ++ show e2
    show (AND e1 e2)    = show e1 ++ " && " ++ show e2
    show (OR e1 e2)     = show e1 ++ " || " ++ show e2
    show (NOT e)        = "¬" ++ show e
    show (IsEmpty v)    = "is empty " ++ show v      
 
-- Commands
data Command a where
    Expr       :: Expression a -> Command a
    Let        :: Variable -> Expression a -> Command a
    Seq        :: Command a -> Command b -> Command b 
    IfThenElse :: Expression Bool -> Command a -> Command b -> Command c  -- Need to destroy the 'Bool'
    ACCUM      :: Command Collection -> Command Collection -> Command Collection    -- Accumulate e1 until e2(is empty)
    REPUNT     :: Command Collection -> Command Collection -> Command Collection

instance Show (Command a) where
    show (Expr e)             = show e
    show (Let v c)            = id v ++ " := " ++ show c
    show (Seq c1 c2)          = show c1 ++ " ; " ++ show c2
    show (IfThenElse b c1 c2) = "if " ++ show b ++ " then " ++ show c1 ++ " else " ++ show c2
    show (ACCUM c1 c2)        = "accumulate " ++ show c1 ++ " until " ++ show c2 
    show (REPUNT c1 c2)       = "repeat " ++ show c1 ++ " until " ++ show c2



