{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

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
    putStrLn "  _    _           _  _______   _                 "
    putStrLn " | |  | |         | |/ /  __ \\ (_)               "
    putStrLn " | |__| | __ _ ___| ' /| |  | | _  ___ ___        "
    putStrLn " |  __  |/ _` / __|  < | |  | || |/ __/ _ \\      "
    putStrLn " | |  | | (_| \\__ \\ . \\| |__| || | (_|  __/    "
    putStrLn " |_|  |_|\\__,_|___/_|\\_\\_____/ |_|\\___\\___|  \n\n"
   
    args <- getArgs
    case args of
        [] -> putStrLn "Error"
        (name:xs) -> execute name

execute :: String -> IO ()
execute name = do
    g <- newStdGen
    file <- readFile $ "../Programs/" ++ name
    case parseComm name file of
        Left error -> print error
        Right t    -> do (print $ eval g t)
                         
