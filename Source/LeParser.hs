-- Solution will run in Haskell Interpreted Mode
module LeParser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST
import Data.Char
import Data.Maybe

------------------------------------------------
-- Funcion para facilitar el testing del parser.
------------------------------------------------

totParser :: Parser a -> Parser a
totParser p = do 
                whiteSpace lis
                t <- p
                eof
                return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                , commentEnd    = "*/"
                                , commentLine   = "//"
                                , opLetter      = char '='
                                , reservedNames = ["true","false","if","then", "least", "largest", "filter", "accum", "repeat", "until",
                                                   "max", "min", "sum", "count", "else", "while", "D", "Z"] 
                                , reservedOpNames = ["-", "+","*", "/", "~", "#", "%", "@@", 
                                                     "==", "/=", "&&", "||", "<", ">", "<=", ">=", "¬", ";", ":="]                   
                                })

----------------------------------
--- Common Parsers
-----------------------------------
parenParse  :: Parser p -> Parser p
parenParse p = do { symbol lis "("
                ; x <- p
                ; symbol lis ")"
                ; return x }

intParse = do
    n <- integer lis
    return (fromInteger n :: Int)

                
-----------------------------------
--- intExpr parsers ---------------
-----------------------------------                  

opParseTerm = do reservedOp lis "+"  
                 return (ADD) 
            <|> do reservedOp lis "-" 
                   return (MINUS) 
         
opParseFactor = do  { reservedOp lis "*" ; return (TIMES) }
              <|> do { reservedOp lis "/" ; return (DIV) }
              <|> do { reservedOp lis "%" ; return (MOD) }

unaryParse = do { reservedOp lis "-" 
                ; b <- factorParse
                ; return (UMINUS b) }
             <|> do { reservedOp lis "~" 
                ; b <- factorParse
                ; return (SGN b) }

intexp :: Parser (Expression Int)
intexp  = do 
    f <- integer lis
    return $ INT (fromInteger f :: Int)


-- ~ intexp :: Parser (Expression a)
intExprParse  = chainl1 termParse opParseTerm
        
-- ~ termParse :: Parser (Expression a)
termParse  = chainl1 factorParse opParseFactor

-- ~ factorParse :: Parser (Expression a)
factorParse  = unaryParse
             <|> try intexp
             <|> parenParse intExprParse

-----------------------------------
--- intExpr parsers ---------------
-----------------------------------                  


     -- ~ SUM     :: Expression a -> Expression Int
     -- ~ COUNT   :: Expression a -> Expression Int




-- ~ diceParse :: Parser (Expression a)
diceParse = do
    k <- intParse
    reserved lis "D"
    n <- intParse
    return (D k n)
    <|> do
    k <- intParse
    reserved lis "Z"
    n <- intParse
    return (Z k n)

-- ~ collParse = do
    -- ~ symbol lis "["
    -- ~ many1 do { x <- intParse <|> x <- intParse (symbol lis ",")}
    -- ~ symbol lis "]"
    
    
     -- ~ INT     :: Int -> Expression Int
     -- ~ COLL    :: Collection -> Expression Collection
     -- ~ BOOL    :: Bool -> Expression Bool
     -- ~ Var     :: Variable -> Expression Value
     -- ~ Least   :: Int -> Expression a -> Expression Collection
     -- ~ Largt   :: Int -> Expression a -> Expression Collection
     -- ~ Filter  :: FilOp -> Expression a -> Expression Collection
     -- ~ Concat  :: Expression a -> Expression b -> Expression Collection
     -- ~ MAX     :: Expression a -> Expression Int
     -- ~ MIN     :: Expression a -> Expression Int
     -- ~ INDEP   :: Expression a -> Expression b -> Expression Collection
     -- ~ Eq      :: Expression a -> Expression b -> Expression Bool
     -- ~ NEq     :: Expression a -> Expression b -> Expression Bool
     -- ~ Lt      :: Expression a -> Expression b -> Expression Bool
     -- ~ Gt      :: Expression a -> Expression b -> Expression Bool
     -- ~ GEt     :: Expression a -> Expression b -> Expression Bool
     -- ~ LEt     :: Expression a -> Expression b -> Expression Bool
     -- ~ AND     :: Expression a -> Expression b -> Expression Bool
     -- ~ OR      :: Expression a -> Expression b -> Expression Bool
     -- ~ NOT     :: Expression a -> Expression Bool
     -- ~ IsEmpty :: Value -> Expression Bool
                    
  
-- ~ varParse  = do x <- identifier lis
               -- ~ return (Var x)
  
  
  -- ~ -----------------------------------
  -- ~ --- Parser de expressiones booleanas
  -- ~ ------------------------------------
  
  -- ~ boolPrimitive :: Parser (Expression a)
  -- ~ boolPrimitive  = do { f <- reserved lis "true"; return (BTrue) }
                   -- ~ <|> do { f <- reserved lis "false"; return (BFalse) }
  
  -- ~ compOpParse = do  { reservedOp lis "==" ; return (Eq) }
                -- ~ <|> do { reservedOp lis "!=" ; return (NEq) }
                -- ~ <|> do { reservedOp lis "<" ; return (Lt) }
                -- ~ <|> do { reservedOp lis ">" ; return (Gt) }
  
  -- ~ boolOpParse = do  { reservedOp lis "&&" ; return (And) }
                -- ~ <|> do { reservedOp lis "||" ; return (Or) }
                
  -- ~ negationParse :: Parser (Expression a)
  -- ~ negationParse  = do { reservedOp lis "¬" 
                      -- ~ ; b <- bTermParse  -- ACA FALTA UN EQUIVALENTE A FACTOR
                      -- ~ ; return (Not b) }
  
  -- ~ comparisonParse :: Parser (Expression a)
  -- ~ comparisonParse  = do { x <- intexp
                        -- ~ ; f <- compOpParse
                        -- ~ ; y <- intexp
                        -- ~ ; return (f x y)}
  
  
  -- ~ boolexp :: Parser (Expression a)
  -- ~ boolexp  = chainl1 bTermParse boolOpParse
  
  -- ~ bTermParse :: Parser (Expression a)
  -- ~ bTermParse  = negationParse
                -- ~ <|> try boolPrimitive
                -- ~ <|> parenParse boolexp
                -- ~ <|> comparisonParse
  
  -- ~ -----------------------------------
  -- ~ --- Parser de comandos
  -- ~ -----------------------------------
  
  -- ~ dacParse  = do  { reservedOp lis ";" ; return (Seq) }

  
  -- ~ letParse :: Parser (Command a)
  -- ~ letParse  = do { name <- identifier lis
                 -- ~ ; reservedOp lis "="
                 -- ~ ; value <- intexp
                 -- ~ ; return (Let name value)}
                 
  -- ~ ifParse :: Parser (Command a)
  -- ~ ifParse  = do { reserved lis "if"
                -- ~ ; cond <- boolexp
                -- ~ ; reserved lis "then"
                -- ~ ; symbol lis "{"
                -- ~ ; thencmd <- comm
                -- ~ ; symbol lis "}"
                -- ~ ; elsecmd <- elseParse
                -- ~ ; return (IfThenElse cond thencmd elsecmd)}
                
  -- ~ elseParse :: Parser (Command a)
  -- ~ elseParse  = do { try (do { reserved lis "else"
                            -- ~ ; symbol lis "{"
                            -- ~ ; elsecmd <- comm
                            -- ~ ; symbol lis "}"
                            -- ~ ; return elsecmd}) }
  

  -- ~ commLine :: Parser (Command a)
  -- ~ commLine  = ifParse
              -- ~ <|> try letParse
              
  -- ~ comm :: Parser (Command a)
  -- ~ comm = chainr1 commLine dacParse
  
  -- ~ ------------------------------------
  -- ~ -- Función de parseo
  -- ~ ------------------------------------
-- ~ parseComm :: SourceName -> String -> Either ParseError (Expression Int)
parseComm :: SourceName -> String -> Either ParseError (Expression Int)
parseComm = parse (totParser intExprParse)
  
  

  
  -- ~ skipParse :: Parser (Command a)
  -- ~ skipParse  = do { reserved lis "skip" 
                 -- ~ ; return (Skip)}
                 
  -- ~ whileParse :: Parser (Command a)
  -- ~ whileParse  = do { reserved lis "while"
                   -- ~ ; cond <- boolexp
                   -- ~ ; symbol lis "{"
                   -- ~ ; cmd <- comm
                   -- ~ ; symbol lis "}"
                   -- ~ ; return (While cond cmd)}
  
