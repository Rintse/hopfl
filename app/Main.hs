module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.Directory

import Control.Monad (when)

import Syntax.Lex
import Syntax.Par
import Syntax.Print
import Syntax.ErrM
import Syntax.Abs as Raw

import Semantics
import Substitution

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

eval :: Verbosity -> ParseFun Raw.Exp -> String -> Raw.Environment -> IO ()
eval v pExp prog env = do
    e <- parse v pExp prog
    -- TODO: What is happening down here V ??? 
    let r = (Ok . evalExp [] e) $ mkEnv env
    --       ^  ^ ^               ^
    --       |  | |               |
    --       Monad constructor    |
    --          | |               |
    --          Composition       |
    --            |               |
    --            m Value         |
    --                            HashMap String Exp
        in case r of
        Bad s -> do
            putStrLn "Evaluation failed"
            putStrLn s
        Ok s -> putStrLn $ "Evaluation result:" ++ show s

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
        let v = if "-s" `elem` args then 0 else 2 in
            case args of
                -- Parse and evaluate with given environment
                "-e":ev:fs  -> parse 1 pEnvironment ev >>= eval v pExp prog
                _ -> do
                    tree <- parse v pExp prog -- Just parse otherwise
                    showTree v (markVars tree) -- And show parsing result



