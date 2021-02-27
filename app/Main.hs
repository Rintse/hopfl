module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.Directory

import Control.Monad (when)

import Syntax.Lex
import Syntax.Par
import Syntax.Print
import Syntax.Abs as Raw

import Semantics
import Syntax.ErrM

import Data.Tree
class Treeish a where
    toTree :: a -> Tree String

type ParseFun a = [Token] -> Err a
myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

-- Prints the parsed AST
-- TODO: actually print a tree
showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
    putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree

-- Starts the parsing and evaluation process
run :: Verbosity -> ParseFun Raw.Exp -> String -> IO ()
run v p s = do
    let ts = myLLexer s in case p ts of
        Bad s -> do 
            putStrLn "\nParse   Failed...\n"
            putStrV v "Tokens:"
            putStrV v $ show ts
            putStrLn s
            exitFailure
        Ok tree -> do 
            putStrLn "\nParse Successful!"
            showTree v tree
            exitSuccess


parse :: (Show a) => Verbosity -> ParseFun a -> String -> IO a
parse v p s = let ts = myLLexer s in 
    case p ts of
        Bad s -> do 
            putStrLn "\nParse Failed...\n"
            putStrV v "Tokens:"
            putStrV v $ show ts
            putStrLn s
            exitFailure
        Ok tree -> do 
            putStrLn "\nParse Successful!"
            return tree

eval :: Verbosity -> ParseFun Raw.Exp -> Raw.Environment -> String -> IO ()
eval v pExp env prog = do
    e <- parse v pExp prog
    let r = Ok . show . evalExp e $ mkEnv env
        in case r of
        Bad s -> do
            putStrLn "Evaluation failed"
            putStrLn s
        Ok s -> putStrLn $ "Evaluation result:" ++ s

-- Prints help message regarding program usage
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

-- Main function
main :: IO ()
main = do
    args <- getArgs
    if "-h" `elem` args then usage
    else if null args then print "Must supply a file"
    else do
        print (last args)
        prog <- readFile $ last args -- Last arg should be the file
        let verbo = if "-s" `elem` args then 0 else 1 in
            case args of
                "-e":envS:fs    ->  parse 1 pEnvironment envS --Parse
                                >>= \env -> eval 1 pExp env prog -- And evaluate
                _               -> run verbo pExp prog -- Just parse otherwise

