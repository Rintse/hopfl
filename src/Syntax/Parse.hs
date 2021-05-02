module Syntax.Parse where

import Syntax.Raw.Par
import Syntax.Raw.Lex
import Syntax.Raw.Abs
import Syntax.Raw.ErrM
import Tools.VerbPrint

import Control.Exception
import Data.Typeable
import System.Exit

type ParseFun a = [Token] -> Err a
myLLexer = myLexer

type ParseMonad a = IO (Either SomeException a)

-- Custom parsing exception
data ParseException 
    = DrawListException 
    | EnvironmentException
    | DepthException
   deriving (Show, Typeable)
instance Exception ParseException

-- Parses contents of given input file
parse :: Bool -> String -> IO Prg
parse v s = do
    putStrV v "Parsing program"
    let ts = myLLexer s
    case pPrg ts of
        Bad r -> do
            putStrLn $"Parse failed: " ++ r
            putStrV v $ "Tokens still in stream:\n" ++ show ts
            exitFailure
        Ok r -> do
            putStrV v "Parse successful"
            return r

-- Parses the environment if such an argument is given
parseEnv :: String -> IO Environment
parseEnv s = case pEnvironment (myLLexer s) of 
    Bad s -> throw EnvironmentException
    Ok ev -> return ev
