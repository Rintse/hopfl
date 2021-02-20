module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import Syntax.Lex
import Syntax.Par
import Syntax.Print
import Syntax.Abs as Raw

import Semantics

usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ unlines 
        [ "usage: " ++ progName ++ " [options] file"
        , "  -h                 Display this help message."
        , "  -s                 Parse silently"
        , "  -e (environment)   Evaluate with given environment"
        ]
    exitFailure



main :: IO ()
main = result


