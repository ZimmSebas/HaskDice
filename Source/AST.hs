{-# LANGUAGE GADTs #-}

module AST where

import Prelude
import System.Random 

---------------------------------------
----- Initial types -------------------
---------------------------------------

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

-- Variable State
type Env = [(Variable,Value)]

-- Typing State of variable (for type eval)
type TypEnv = [(Variable, Type)]

-- Result of an Eval (for instance show purposes)
data EvalResult = ER (Value, Env, StdGen)

instance Show EvalResult where
    show (ER (val,st,stdg)) = "Result: " ++ show val ++ "\nVariables: " ++ show st ++ "\n"
 
 
-- Possible errors that the program may have
data Error = TypingError Type Type String
           | VarNotExist String 
           | DivByZero String String 
           | ModByZero String String
           | PatternMatchError 

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
    show (PatternMatchError) = "An error ocurred in the error pattern match, this should never happen. Contact the developer\n"

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

data Expression where 
     D       :: Int -> Int -> Expression
     Z       :: Int -> Int -> Expression
     INT     :: Int -> Expression
     COLL    :: Collection -> Expression
     BOOL    :: Bool -> Expression
     Var     :: Variable -> Expression
     Least   :: Int -> Expression -> Expression 
     Largt   :: Int -> Expression -> Expression 
     Filter  :: FilOp -> Expression -> Expression
     Concat  :: Expression -> Expression -> Expression
     MAX     :: Expression -> Expression
     MIN     :: Expression -> Expression
     SUM     :: Expression -> Expression
     COUNT   :: Expression -> Expression
     ADD     :: Expression -> Expression -> Expression
     MINUS   :: Expression -> Expression -> Expression
     TIMES   :: Expression -> Expression -> Expression
     DIV     :: Expression -> Expression -> Expression
     MOD     :: Expression -> Expression -> Expression
     UMINUS  :: Expression -> Expression 
     SGN     :: Expression -> Expression
     INDEP   :: Expression -> Expression -> Expression
     Eq      :: Expression -> Expression -> Expression
     NEq     :: Expression -> Expression -> Expression
     Lt      :: Expression -> Expression -> Expression
     Gt      :: Expression -> Expression -> Expression
     GEt     :: Expression -> Expression -> Expression
     LEt     :: Expression -> Expression -> Expression
     AND     :: Expression -> Expression -> Expression
     OR      :: Expression -> Expression -> Expression
     NOT     :: Expression -> Expression
     IsEmpty :: Value -> Expression
 
instance Show Expression where
    show (D k n)        = show k ++ "D" ++ show n 
    show (Z k n)        = show k ++ "Z" ++ show n
    show (INT i)        = show i
    show (COLL c)       = show c
    show (Var v)        = id v
    show (Least i c)    = "least " ++ show i ++ " " ++ show c
    show (Largt i c)    = "largest " ++ show i ++ " " ++ show c
    show (Filter fop c) = "filter " ++ show fop ++ " " ++ show c
    show (Concat c1 c2) = show c1 ++ " @@ " ++ show c2
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
data Command where
    Expr       :: Expression -> Command
    Let        :: Variable -> Expression -> Command
    Seq        :: Command -> Command -> Command 
    IfThenElse :: Expression -> Command -> Command -> Command 
    ACCUM      :: Command -> Command -> Command 
    REPUNT     :: Command -> Command -> Command

instance Show Command where
    show (Expr e)             = show e
    show (Let v c)            = id v ++ " := " ++ show c
    show (Seq c1 c2)          = show c1 ++ ";\n" ++ show c2
    show (IfThenElse b c1 c2) = "if " ++ show b ++ " then " ++ show c1 ++ " else " ++ show c2
    show (ACCUM c1 c2)        = "accumulate " ++ show c1 ++ " until " ++ show c2 
    show (REPUNT c1 c2)       = "repeat " ++ show c1 ++ " until " ++ show c2
