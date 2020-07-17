module LeParser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST
import Data.Char
import Data.Maybe

------------------------------------------------
--- Token analizer and Total Parser ------------
------------------------------------------------

totParser :: Parser a -> Parser a
totParser p = do 
                whiteSpace lis
                t <- p
                eof
                return t


-- Token analizer
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "{-"
                                , commentEnd    = "-}"
                                , commentLine   = "--"
                                , opLetter      = char '='
                                , reservedNames = ["True","False","if","then", "least", "largest", "filter", "accum", "repeat", "until",
                                                   "max", "min", "sum", "count", "else", "while", ":=" ] 
                                , reservedOpNames = ["-", "+","*", "/", "~", "#", "%", "@@", 
                                                     "==", "/=", "&&", "||", "<", ">", "<=", ">=", ";", "¬", "D", "Z"]                   
                                , caseSensitive = True
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


varParse :: Parser Expression
varParse = do name <- identifier lis
              return (Var name)
              

                
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

intexp :: Parser Expression
intexp  = do 
    f <- integer lis
    return $ INT (fromInteger f :: Int)


opCollIntParse :: Parser Expression
opCollIntParse = do reserved lis "max"
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

factorParse :: Parser Expression
factorParse  = unaryParse
             <|> try intexp
             <|> try (parenParse intExprParse)
             <|> opCollIntParse
             <|> try varParse


termParse :: Parser Expression
termParse  = chainl1 factorParse opParseFactor

intExprParse :: Parser Expression 
intExprParse  = chainl1 termParse opParseTerm

-------------------------------------
--- CollExps Parser -----------------
-------------------------------------

diceParse :: Parser Expression 
diceParse = do k <- intParse
               reservedOp lis "D"
               n <- intParse
               return (D k n)
            <|> do k <- intParse
                   reservedOp lis "Z"
                   n <- intParse
                   return (Z k n)

collParse :: Parser Expression
collParse = do
    symbol lis "["
    c <- sepBy1 intParse (symbol lis ",")
    symbol lis "]"
    return (COLL c)

concatParse =  do reservedOp lis "@@"
                  return (Concat) 

indepParse = do { try (do n <- intExprParse
                          reservedOp lis "#"
                          c <- collExprParse
                          return (INDEP n c)) }    

filOpParse :: Parser FilOp
filOpParse = try (do symbol lis "("
                     symbol lis ">"
                     n <- intParse
                     symbol lis ")"
                     return (Grtth n) ) 
             <|> try (do symbol lis "("
                         symbol lis "<"
                         n <- intParse
                         symbol lis ")"
                         return (Lowth n) ) 
             <|> try (do symbol lis "("
                         symbol lis "<"
                         symbol lis "="
                         n <- intParse
                         symbol lis ")" 
                         return (LowEqt n) ) 
             <|> try (do symbol lis "("
                         symbol lis ">"
                         symbol lis "="
                         n <- intParse
                         symbol lis ")"
                         return (GrtEqt n) ) 
             <|> try (do symbol lis "("
                         symbol lis "="
                         symbol lis "="
                         n <- intParse
                         symbol lis ")"
                         return (Equal n) ) 
             <|> try (do symbol lis "("
                         symbol lis "/"
                         symbol lis "="
                         n <- intParse
                         symbol lis ")"
                         return (NEqual n) ) 
                   
opCollCollParse :: Parser Expression
opCollCollParse = do reserved lis "least"
                     n <- intParse
                     c <- collTermParse
                     return (Least n c)
                  <|> do reserved lis "largest"
                         n <- intParse
                         c <- collTermParse
                         return (Largt n c)
                  <|> do reserved lis "filter"
                         fop <- filOpParse
                         c <- collTermParse
                         return (Filter fop c)        
                         


collTermParse :: Parser Expression
collTermParse = diceParse
                <|> collParse
                <|> indepParse
                <|> opCollCollParse
                <|> try (parenParse collExprParse)
                <|> try varParse
                
collExprParse :: Parser Expression
collExprParse =  chainl1 collTermParse concatParse

               
                
-----------------------------------
--- BoolExps Parse ----------------
-----------------------------------
  
boolPrimitive :: Parser Expression
boolPrimitive  = do { f <- reserved lis "True"; return (BOOL True) }
               <|> do { f <- reserved lis "False"; return (BOOL False) }


compOpParse = do  { reservedOp lis "==" ; return (Eq) }
              <|> do { reservedOp lis "/=" ; return (NEq) }
              <|> do { reservedOp lis "<" ; return (Lt) }
              <|> do { reservedOp lis ">" ; return (Gt) }
              <|> do { reservedOp lis "<=" ; return (LEt) }
              <|> do { reservedOp lis ">=" ; return (GEt) }

boolOpParse = do  { reservedOp lis "&&" ; return (AND) }
              <|> do { reservedOp lis "||" ; return (OR) }
            
negationParse :: Parser Expression
negationParse  = do { reservedOp lis "¬" 
                  ; b <- bTermParse 
                  ; return (NOT b) }

comparisonParse :: Parser Expression
comparisonParse  = do { x <- intExprParse
                    ; f <- compOpParse
                    ; y <- intExprParse
                    ; return (f x y)}


boolParse :: Parser Expression
boolParse  = chainl1 bTermParse boolOpParse

bTermParse :: Parser Expression
bTermParse  = negationParse
            <|> try boolPrimitive
            <|> try (parenParse boolParse)
            <|> comparisonParse
            <|> try varParse

-------------------------------------
--- General Expressions Parser ------
-------------------------------------

expParse :: Parser Expression
expParse = try collExprParse 
           <|> try boolParse 
           <|> try intExprParse
           <|> try varParse


-------------------------------------
--- Command Parser ------------------
-------------------------------------

dacParse :: Parser (Command -> Command -> Command)
dacParse  = do  { reservedOp lis ";" ; return (Seq) }                    
  
letParse :: Parser Command
letParse  = do reserved lis "let"
               name <- identifier lis
               reservedOp lis ":="
               value <- expParse
               return (Let name value)
               
exprParse = do e <- expParse
               return (Expr e)
                 
ifElseParse :: Parser Command
ifElseParse  = do reserved lis "if"
                  cond <- boolParse
                  reserved lis "then"
                  thencmd <- commParse
                  reserved lis "else"
                  elsecmd <- commParse
                  return (IfThenElse cond thencmd elsecmd)

recursiveCommParse :: Parser Command
recursiveCommParse = do reserved lis "repeat"
                        name <- identifier lis
                        reserved lis ":="
                        value <- expParse
                        reserved lis "until"
                        c <- commLine
                        return (REPUNT (Let name value) c)
                     <|> do reserved lis "accum"
                            name <- identifier lis
                            reserved lis ":="
                            value <- expParse
                            reserved lis "until"
                            c <- commLine
                            return (ACCUM (Let name value) c)


commLine :: Parser Command
commLine  = ifElseParse
          <|> letParse
          <|> try exprParse
          <|> parenParse commParse
          <|> recursiveCommParse
          

commParse :: Parser Command
commParse = chainr1 commLine dacParse

-- ~ ifParse :: Parser Command
-- ~ ifParse  = do { reserved lis "if"
            -- ~ ; cond <- exprParse
            -- ~ ; reserved lis "then"
            -- ~ ; thencmd <- commParse
            -- ~ ; elsecmd <- elseParse
            -- ~ ; return (IfThenElse cond thencmd elsecmd)}
           
-- ~ elseParse :: Parser Command
-- ~ elseParse  = do { try (do { reserved lis "else"
                        -- ~ ; symbol lis "{"
                        -- ~ ; elsecmd <- comm
                        -- ~ ; symbol lis "}"
                        -- ~ ; return elsecmd}) }
  

  
------------------------------------
-- Función de parseo
------------------------------------

parseComm :: SourceName -> String -> Either ParseError Command
parseComm = parse (totParser commParse)
  
  

