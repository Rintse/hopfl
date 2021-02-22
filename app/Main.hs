module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import Control.Monad (when)

import Syntax.Lex
import Syntax.Par
import Syntax.Print
import Syntax.Abs as Raw

import Semantics
import ErrM

import Data.Tree
class Treeish a where
  toTree :: a -> Tree String

type ParseFun a = [Token] -> Err a
myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s


-- Read and parse file. Pass the AST to the eval function
evalFile :: Verbosity -> ParseFun Raw.Exp -> String -> Environment -> FilePath -> IO ()
evalFile v pExp sem env f = putStrLn f >> readFile f >>= eval v pExp sem env


-- Evaluates the semantics of an AST
eval :: Verbosity -> ParseFun Raw.Exp -> Raw.Environment -> String -> IO ()
eval v pExp env prog = do
    e <- parse v (fmap toDeBruijnTree . pExp) prog
    let r = Ok . show . Big.evalExp e $ Big.mkEnv env
        in case r of
        Bad s -> do
            putStrLn "Evaluation failed"
            putStrLn s
        Ok s -> putStrLn $ "Evaluation result:" ++ s


-- Prints the parsed AST
showTree :: (Show a, Print a, Treeish a) => Int -> a -> IO ()
showTree v tree = do
    putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrV v $ "\n[Syntax tree]\n\n"     ++ (drawTree $ toTree tree)
    putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree


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



main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-h"] -> usage
    "-s":fs -> mapM_ (runFile 0 pExp) fs
    "-e":s:envS:fs -> parse 2 pEnvironment envS >>= \env -> mapM_ (evalFile 2 pExp s env) fs
    fs -> mapM_ (runFile 2 pExp) fs

