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

main :: IO ()
main = do
    args <- getArgs
    execute args

execute :: String -> IO ()
execute file = do
    let lexeado = ALexer.lexer file in
    print lexeado
