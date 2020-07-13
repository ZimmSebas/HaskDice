{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Test where

import AST
import TypeEval
import RandomState
import Eval
import ALexer 
import Prelude
import System.Environment

-- ~ main :: IO ()
-- ~ main = do
    -- ~ args <- getArgs
    -- ~ case args of
        -- ~ [] -> putStrLn "Error"
        -- ~ (name:xs) -> execute name

main :: IO ()
main = do
    execute "test.hkd"

execute :: String -> IO ()
execute name = do
    file <- readFile $ "../Programs/" ++ name
    let lexeado = (ALexer.lexer) file in
        print lexeado
