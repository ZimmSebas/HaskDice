module TestParse where

import System.Environment (getArgs)
import LeParser (parseComm)
import Eval
import AST
import System.IO
import System.Random 

-- Modificar este import para usar diferentes evaluadores
---------------------------------------------------------

main :: IO ()
main = do arg:_ <- getArgs
          run arg

-- Ejecuta un programa a partir de su archivo fuente
-- ~ run :: [Char] -> IO ()
run name = do
    g <- newStdGen
    file <- readFile $ "../Programs/" ++ name
    case parseComm name file of
      Left error -> print error
      Right t    -> do {print t ; print (eval g t) } --imprimir sin evaluar (para testear Parser)
      -- ~ Right t    -> print (eval t) --imprimir el resultado de evaluar.
     
      
