{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Exception (catch,IOException)
import System.IO.Error (catchIOError)
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

iprompt :: String
iprompt = "HkD> "

ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing


data State = S { inter :: Bool,      -- True, if interactive mode
               lfile :: String,      -- Last file loaded (to "reload")
               env :: Env,           -- Enviroment with variables and values  [Variable,Value]
               stdg :: StdGen        -- Random number generator
             }


interactiveMode :: [String] -> IO ()
interactiveMode []        = do
        haskdicelogo
        g <- newStdGen
        readevalprint g [] -- "infinite" loop with random number generator + empty state (no variables yet)

readevalprint :: StdGen -> Env -> IO ()
readevalprint g st = do maybeline <- catch (readline iprompt) ioExceptionCatcher
                        case maybeline of
                             Nothing     -> print "Error reading something?"
                             Just ""     -> readevalprint g st
                             Just "exit" -> print "Goodbye! Have fun!"
                             Just line   -> case (parseInt line) of
                                                 (Left e) -> do {print e; readevalprint g st}
                                                 (Right res) -> case eval g st res of 
                                                                     Crash e                     -> do {print e ; readevalprint g st} 
                                                                     Return reval@(ER (value, state, stdg)) -> do {print reval ; readevalprint stdg state}
                                                
