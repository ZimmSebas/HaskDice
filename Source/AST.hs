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
 
-- Possible errors that the program may have
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

-- The result of an evaluation (type or value)
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



-- FilOp representa operadores de filter.
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
     D       :: Int -> Int -> Expression c
     Z       :: Int -> Int -> Expression c
     INT     :: Int -> Expression n
     COLL    :: Collection -> Expression c
     BOOL    :: Bool -> Expression b
     Var     :: Variable -> Expression v
     Least   :: Int -> Expression c1 -> Expression c2
     Largt   :: Int -> Expression c1 -> Expression c2
     Filter  :: FilOp -> Expression c1 -> Expression c2
     Concat  :: Expression c1 -> Expression c2 -> Expression c3
     MAX     :: Expression c -> Expression n
     MIN     :: Expression c -> Expression n
     SUM     :: Expression c -> Expression n
     COUNT   :: Expression c -> Expression n
     ADD     :: Expression x -> Expression y -> Expression n
     MINUS   :: Expression x -> Expression y -> Expression n
     TIMES   :: Expression x -> Expression y -> Expression n
     DIV     :: Expression x -> Expression y -> Expression n
     MOD     :: Expression x -> Expression y -> Expression n
     UMINUS  :: Expression x -> Expression n 
     SGN     :: Expression x -> Expression n
     INDEP   :: Expression n -> Expression c1 -> Expression c2
     Eq      :: Expression x -> Expression y -> Expression b
     NEq     :: Expression x -> Expression y -> Expression b
     Lt      :: Expression x -> Expression y -> Expression b
     Gt      :: Expression x -> Expression y -> Expression b
     GEt     :: Expression x -> Expression y -> Expression b
     LEt     :: Expression x -> Expression y -> Expression b
     AND     :: Expression p -> Expression q -> Expression b
     OR      :: Expression p -> Expression q -> Expression b
     NOT     :: Expression p -> Expression b
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
    IfThenElse :: Expression Bool -> Command a -> Command b -> Command c 
    ACCUM      :: Command c1 -> Command v -> Command c2 
    REPUNT     :: Command c1 -> Command v -> Command c2

instance Show (Command a) where
    show (Expr e)             = show e
    show (Let v c)            = id v ++ " := " ++ show c
    show (Seq c1 c2)          = show c1 ++ ";\n" ++ show c2
    show (IfThenElse b c1 c2) = "if " ++ show b ++ " then " ++ show c1 ++ " else " ++ show c2
    show (ACCUM c1 c2)        = "accumulate " ++ show c1 ++ " until " ++ show c2 
    show (REPUNT c1 c2)       = "repeat " ++ show c1 ++ " until " ++ show c2
