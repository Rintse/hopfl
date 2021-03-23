module Main where

import Syntax.Lex
import Syntax.Par
import Syntax.Print
import Syntax.ErrM
import Syntax.Abs
import Semantics
import Substitution
import Treeify

import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.Directory
import Data.List.Split
import Data.Maybe
import Data.Char
import Text.Read
import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.State
import Debug.Trace

type ParseFun a = [Token] -> Err a
myLLexer = myLexer

-- Parses a user-provided random-draws list
parseList :: String -> Err [Double]
parseList s = let l = filter (not . all isSpace) $ splitOn "," s 
    in let parsed = map (readMaybe :: String -> Maybe Double) l
    in if Nothing `elem` parsed then Bad [] else Ok (catMaybes parsed)

type Verbosity = Int
putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 0) $ putStrLn s

-- Prints the parsed AST
-- TODO: actually print a tree
showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = putStrV v $ "[Abstract Syntax]\n" ++ show tree ++ "\n"

-- Parses both environments and programs
parse :: (Show a) => Verbosity -> ParseFun a -> String -> IO a
parse v p s = let ts = myLLexer s in
    case p ts of
        Bad s -> do
            putStrLn "Parse Failed...\n"
            putStrV v "Tokens:"
            putStrV v $ show ts
            putStrLn s
            exitFailure
        Ok tree -> do
            putStrLn "Parse Successful!\n"
            return tree

-- Evaluates a program given a maximum eval depth, and environment
eval :: Verbosity -> Exp -> Integer -> [Double] -> Environment -> IO ()
eval v prog n s env = do
    putStrLn ("Evaluating with random draws: " ++ show s ++ "\n")
    let r = runStateT (runReaderT (runSem (evalExp prog)) (mkEnv env, n)) (1.0, s)
        in case r of
        Bad s -> do
            putStrLn "Evaluation failed"
            putStrLn s
        Ok (s, (w,_)) -> putStrLn $ 
            "\nEvaluation result (with density " ++ show w ++ "): \n" ++ show s

-- Prints help message regarding program usage
usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ unlines
        [ "usage: " ++ progName ++ " [options] file"
        , "  -h                                 Display this help message."
        , "  -s                                 Parse silently"
        , "  -t                                 Just show a tree of the program"
        , "  -e (environment) depth draws       Evaluate with environment " ++
            "up to depth using optional comma seperated draws"
        ]
    exitFailure


-- Main function
main :: IO ()
main = do
    args <- getArgs
    if "-h" `elem` args then usage
    else if null args then putStrLn "Must supply a file"
    else let v =  (if "-s" `elem` args then 0 else 1) in do
        file <- readFile $ last args -- Last arg should be the file
        putStrLn "Parsing program"
        prog <- parse v pExp file
        showTree v (uniqNames prog)

        case args of
            -- Parse and evaluate with given environment
            "-e":e:n:l:f -> do
                putStrLn "Parsing environment"
                env <- parse 0 pEnvironment e
                case readMaybe n of
                    Just n  -> case parseList l of 
                        Ok s -> do
                            eval v (uniqNames prog) n s env
                        _    -> putStrLn "Invalid draws list"
                    Nothing -> putStrLn "Invalid depth"
            "-t":f -> putStrLn $ treeTerm prog
            _ -> putStrLn "Invalid usage"


