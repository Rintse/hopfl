module Main where

import Args
import Syntax.Parse
import Preprocess.Preprocess
import Semantics.Evaluation
import Tools.Treeify

import Control.Monad.Reader
import System.Environment ( getArgs )
import System.Console.GetOpt
import System.Exit

-- Parses the arguments, input and performs the requested actions
main :: IO ()
main = do
    args <- getArgs -- Get and parse options
    let (optArgs, nonOpts, errs) = getOpt RequireOrder Args.options args

    -- Errors parsing arguments
    unless (null errs) ( do
        putStrLn "Errors parsing arguments:"
        mapM_ putStr errs >> exitFailure )

    opts <- foldl (>>=) (return defaultOpts) optArgs

    let Options {   optVerbose  = verb,     optInput    = input,
                    optEval     = eval,     optEnv      = env,
                    optDraws    = draws,    optDepth    = depth     } = opts
    
    -- Parse input into a program AST
    prog <- input >>= parse verb
   
    -- Preprocess raw AST into one expression
    exp <- preprocess prog env

    -- Show the result
    showProg verb exp

    -- Evaluate if requested
    when eval $ evaluate verb exp depth draws env

