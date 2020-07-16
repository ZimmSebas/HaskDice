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
                                                     "==", "/=", "&&", "||", "<", ">", "<=", ">=", ";", "¬", ":="]                   
                                })

-----------------------------------
--- Common Parsers ----------------
-----------------------------------
parenParse  :: Parser p -> Parser p
parenParse p = do { symbol lis "("
                ; x <- p
                ; symbol lis ")"
                ; return x }

intParse = do
    n <- integer lis
    return (fromInteger n :: Int)


                
-------------------------------------
--- IntExps Parser ------------------
-------------------------------------


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

intexp :: Parser (Expression a)
intexp  = do 
    f <- integer lis
    return $ INT (fromInteger f :: Int)


opCollParse :: Parser (Expression a)
opCollParse = do reserved lis "max"
                 c <- collExprParse 
                 return (MAX c)
              <|> do reserved lis "min"
                     c <- collExprParse 
                     return (MIN c)
              <|> do reserved lis "sum"
                     c <- collExprParse 
                     return (SUM c)
              <|> do reserved lis "count"
                     c <- collExprParse 
                     return (COUNT c)

factorParse :: Parser (Expression a)
factorParse  = unaryParse
             <|> try intexp
             <|> try (parenParse intExprParse)
             <|> opCollParse


termParse :: Parser (Expression a)
termParse  = chainl1 factorParse opParseFactor

intExprParse :: Parser (Expression a)
intExprParse  = chainl1 termParse opParseTerm

-------------------------------------
--- CollExps Parser -----------------
-------------------------------------

diceParse :: Parser (Expression a) -- Bug, need a sepBy once. Doesn't work
diceParse = do k <- intParse
               reserved lis "D"
               n <- intParse
               return (D k n)
            <|> do k <- intParse
                   reserved lis "Z"
                   n <- intParse
                   return (Z k n)

collParse :: Parser (Expression a)
collParse = do
    symbol lis "["
    c <- sepBy1 intParse (symbol lis ",")
    symbol lis "]"
    return (COLL c)


collExprParse :: Parser (Expression a)
collExprParse = diceParse
                <|> collParse

-----------------------------------
--- BoolExps Parse ----------------
-----------------------------------
  
boolPrimitive :: Parser (Expression a)
boolPrimitive  = do { f <- reserved lis "true"; return (BOOL True) }
               <|> do { f <- reserved lis "false"; return (BOOL False) }


compOpParse = do  { reservedOp lis "==" ; return (Eq) }
              <|> do { reservedOp lis "/=" ; return (NEq) }
              <|> do { reservedOp lis "<" ; return (Lt) }
              <|> do { reservedOp lis ">" ; return (Gt) }
              <|> do { reservedOp lis "<=" ; return (LEt) }
              <|> do { reservedOp lis ">=" ; return (GEt) }

boolOpParse = do  { reservedOp lis "&&" ; return (AND) }
              <|> do { reservedOp lis "||" ; return (OR) }
            
negationParse :: Parser (Expression a)
negationParse  = do { reservedOp lis "¬" 
                  ; b <- bTermParse 
                  ; return (NOT b) }

comparisonParse :: Parser (Expression a)
comparisonParse  = do { x <- intExprParse
                    ; f <- compOpParse
                    ; y <- intExprParse
                    ; return (f x y)}


boolParse :: Parser (Expression a)
boolParse  = chainl1 bTermParse boolOpParse

bTermParse :: Parser (Expression a)
bTermParse  = negationParse
            <|> try boolPrimitive
            <|> try (parenParse boolParse)
            <|> comparisonParse

-------------------------------------
--- General Expressions Parser ------
-------------------------------------


-- ~ varParse :: Parser (Expression a)
-- ~ varParse = do name <- identifier lis
              -- ~ return (Var name)
              

expParse :: Parser (Expression a)
expParse = try collExprParse 
           <|> try intExprParse
           <|> boolParse
           -- ~ <|> try varParse




-------------------------------------
--- Command Parser ------------------
-------------------------------------

dacParse :: Parser (Command a -> Command b -> Command b)
dacParse  = do  { reservedOp lis ";" ; return (Seq) }                    
  
letParse :: Parser (Command a)
letParse  = do reserved lis "let"
               name <- identifier lis
               reservedOp lis ":="
               value <- expParse
               return (Let name value)
               
exprParse = do e <- expParse
               return (Expr e)
                 
ifElseParse :: Parser (Command a)
ifElseParse  = do reserved lis "if"
                  cond <- boolParse
                  reserved lis "then"
                  thencmd <- commParse
                  reserved lis "else"
                  elsecmd <- commParse
                  return (IfThenElse cond thencmd elsecmd)

commLine :: Parser (Command a)
commLine  = ifElseParse
          <|> letParse
          <|> try exprParse
          <|> parenParse commParse

commParse :: Parser (Command a)
commParse = chainr1 commLine dacParse

-- ~ ifParse :: Parser (Command a)
-- ~ ifParse  = do { reserved lis "if"
            -- ~ ; cond <- exprParse
            -- ~ ; reserved lis "then"
            -- ~ ; thencmd <- commParse
            -- ~ ; elsecmd <- elseParse
            -- ~ ; return (IfThenElse cond thencmd elsecmd)}
           
-- ~ elseParse :: Parser (Command a)
-- ~ elseParse  = do { try (do { reserved lis "else"
                        -- ~ ; symbol lis "{"
                        -- ~ ; elsecmd <- comm
                        -- ~ ; symbol lis "}"
                        -- ~ ; return elsecmd}) }
  

  
  -- ~ ------------------------------------
  -- ~ -- Función de parseo
  -- ~ ------------------------------------
-- ~ parseComm :: SourceName -> String -> Either ParseError (Expression a)
parseComm :: SourceName -> String -> Either ParseError (Command a)
parseComm = parse (totParser commParse)
  
  

  
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
  
