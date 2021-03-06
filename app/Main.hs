{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.Directory
import Text.Read

import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.State

import Syntax.Lex
import Syntax.Par
import Syntax.Print
import Syntax.ErrM
import Syntax.Abs as Raw
import Semantics
import Substitution
import RandomList

type ParseFun a = [Token] -> Err a
myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 0) $ putStrLn s

-- Prints the parsed AST
-- TODO: actually print a tree
showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
    putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree


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

eval :: Verbosity -> ParseFun Raw.Exp -> String -> Integer -> Environment -> IO ()
eval v pExp prog n env = do
    e <- parse v pExp prog
    let s = genList e in do
        putStrLn ("Evaluating with random draws: " ++ show s)
        let r = runReaderT (evalStateT (runSem (evalExp n e)) (1.0, s)) (mkEnv env)
            in case r of
            Bad s -> do
                putStrLn "Evaluation failed"
                putStrLn s
            Ok s -> putStrLn $ "Evaluation result (with density ??): \n" ++ show s

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
    else if null args then putStrLn "Must supply a file"
    else do
        prog <- readFile $ last args -- Last arg should be the file
        let v = if "-s" `elem` args then 0 else 1 in
            case args of
                -- Parse and evaluate with given environment
                "-e":e:n:f -> do
                    env <- parse 0 pEnvironment e
                    let depth = readMaybe n in
                        case depth of
                            Just n  -> eval v pExp prog n env
                            Nothing -> putStrLn "Invalid depth"
                _ -> do
                    tree <- parse v pExp prog -- Just parse otherwise
                    showTree v (uniqNames tree) -- And show parsing result



