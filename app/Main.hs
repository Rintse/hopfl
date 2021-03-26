module Main where

import Syntax.Par
import Syntax.Abs
import Syntax.ErrM
import Semantics.Evaluation
import Semantics.Substitution
import Tools.VerbPrint
import Tools.Treeify
import Args

import System.Environment ( getArgs )
import System.Console.GetOpt
import Control.Monad.Except
import System.Exit

-- Parses contents of given input file
parse :: Bool -> String -> IO Exp
parse v s = do
    putStrV v "Parsing program"
    let ts = myLLexer s
    case pExp ts of
        Bad r -> do 
            putStrLn $"Parse failed: " ++ r
            exitFailure
        Ok r -> do 
            putStrV v "Parse successful"
            showProg v r
            return r

-- Parses the arguments, input and performs the requested actions
main :: IO ()
main = do
    args <- getArgs -- Get and parse options
    let (optArgs, nonOpts, errs) = getOpt RequireOrder Args.options args
    opts <- foldl (>>=) (return defaultOpts) optArgs

    let Options {   optVerbose  = verb,
                    optInput    = input,
                    optEval     = eval,
                    optEnv      = env,
                    optDraws    = draws,
                    optDepth    = depth     } = opts

    -- Parse input
    prog <- parse verb input
    -- Evaluate if requested
    when eval $ evaluate verb (uniqNames prog) depth draws env
