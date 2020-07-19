{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Exception (catch,IOException)
-- ~ import Control.Monad.Except
import System.Console.Readline (readline)

import AST
import TypeEval
import RandomState
import Eval
import LeParser
import Prelude
import System.Environment
import System.Random

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Error"
        ("-i":xs) -> interactiveMode xs
        (name:xs) -> executeFile name

executeFile :: String -> IO ()
executeFile name = do
    g <- newStdGen
    file <- readFile $ "../Programs/" ++ name
    case parseFile name file of
        Left error -> print error
        Right t    -> do (print $ eval g [] t)


haskdicelogo :: IO ()
haskdicelogo = do
    putStrLn "  _    _           _  _______   _                 "
    putStrLn " | |  | |         | |/ /  __ \\ (_)               "
    putStrLn " | |__| | __ _ ___| ' /| |  | | _  ___ ___        "
    putStrLn " |  __  |/ _` / __|  < | |  | || |/ __/ _ \\      "
    putStrLn " | |  | | (_| \\__ \\ . \\| |__| || | (_|  __/    "
    putStrLn " |_|  |_|\\__,_|___/_|\\_\\_____/ |_|\\___\\___|  \n\n"

iprompt, iname :: String
iprompt = "HkD>"
iname   = "HasKDice"

data State = S { inter :: Bool,      -- True, si estamos en modo interactivo.
               lfile :: String,      -- Ultimo archivo cargado (para hacer "reload")
               env :: Env,           -- Entorno con variables globales y su valor  [(Name, (Value, Type))]
               stdg :: StdGen        -- Generador de números aleatorios
             }


interactiveMode :: [String] -> IO ()
interactiveMode []        = do
        putStrLn "\nWelcome to:"
        haskdicelogo
        g <- newStdGen
        readevalprint g []

readevalprint :: StdGen -> Env -> IO ()
readevalprint g st = do maybeline <- readline iprompt
                        case maybeline of
                             Nothing -> print "Error reading something?"
                             Just line -> case (parseInt line) of
                                               (Left e) -> do {print e; readevalprint g st}
                                               (Right res) -> case eval g st res of -- make case in parseInt
                                                                   Crash e                     -> do {print e ; readevalprint g st} 
                                                                   Return (value, state, stdg) -> do {print (Return (value,state)) ; readevalprint stdg state}
                                                
