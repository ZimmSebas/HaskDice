{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Exception (catch,IOException)
import System.IO.Error 
import qualified System.Console.Readline (readline)
import Data.List
import Data.Char

import AST
import TypeEval
import RandomState
import Eval
import LexerParser
import Prelude
import System.Environment
import System.Random

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Error"
        ("-i":xs) -> interactiveMode xs
        (name:xs) -> executeFile False [] name

executeFile :: Bool -> Env -> String -> IO ()
executeFile inter st name = do
    g <- newStdGen
    file <- readFile name
    case parseFile name file of
        Left error -> print error
        Right t    -> do case eval g [] t of 
                              Crash er -> do print er  
                                             if inter then readevalprint g st else putStr "\n Finished with error\n"
                              Return reval@(ER (value, state, stdg)) -> do print reval 
                                                                           if inter then readevalprint stdg state 
                                                                                    else putStr "\n Finished sucessfully! \n"

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
ioExceptionCatcher e = if (isEOFError e) then do {print "Goodbye! See you soon!"; return Nothing} else do {print "IOError reached" ; return Nothing}

helpText :: String
helpText = "\n Welcome to HaskDice! Commands available right now: \n\n" ++
           "- :load / :l  <file> loads a file \n" ++
           "- :quit / :q  Quits the interactive mode \n" ++
           "- :help / :?  You are here so.. you know.. helps (?)\n" ++
           "Have a pleasant day! \n\n"

interactiveMode :: [String] -> IO ()
interactiveMode []        = do
        haskdicelogo
        g <- newStdGen
        readevalprint g [] -- "infinite" loop with random number generator + empty state (no variables yet)

readevalprint :: StdGen -> Env -> IO ()
readevalprint g st = do maybeline <- catchIOError (System.Console.Readline.readline iprompt) ioExceptionCatcher
                        case maybeline of
                             Nothing      -> putStr "Unexpected error\n"
                             Just ""      -> readevalprint g st
                             Just ":q"    -> putStr "Goodbye! See you soon! :D\n"
                             Just ":quit" -> putStr "Goodbye! See you soon! :D\n"
                             Just ":help" -> do {putStr helpText ; readevalprint g st}
                             Just ":?"    -> do {putStr helpText ; readevalprint g st}
                             Just line    -> loadOrInter line g st

loadOrInter :: String -> StdGen -> Env -> IO ()
loadOrInter line g st = if isPrefixOf ":l" line then do let (_,t') =  break isSpace line
                                                            t      =  dropWhile isSpace t'
                                                        executeFile True st t
                                                else case (parseInt line) of
                                                          (Left e) -> do {print e; readevalprint g st}
                                                          (Right res) -> case eval g st res of 
                                                                              Crash e                     -> do {print e ; readevalprint g st} 
                                                                              Return reval@(ER (value, state, stdg)) -> do {print reval ; readevalprint stdg state}
                                                                              
