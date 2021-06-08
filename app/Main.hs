module Main where

import Args
import Syntax.Parse
import Semantics.Evaluation
import Preprocess.Definitions
import Preprocess.AnnotateVars
import Tools.Treeify

import Control.Monad.Reader
import System.Environment ( getArgs )
import System.Console.GetOpt
import System.Exit

main :: IO ()
main = do
    args <- getArgs -- Get and parse options
    let (optArgs, nonOpts, errs) = getOpt RequireOrder Args.options args

    -- Errors parsing arguments
    unless (null errs) ( do
        putStrLn "The were errors parsing the arguments:"
        mapM_ putStr errs >> exitFailure )

    opts <- foldl (>>=) (return defaultOpts) optArgs

    let Options {   optVerbose  = verb,     optInput    = input,
                    optEval     = eval,     optEnv      = env,
                    optDraws    = draws,    optDepth    = depth     } = opts
    
    -- Parse input into a program AST
    prog <- input >>= parse verb
   
    -- Preprocess raw AST into one expression
    withDefinitions <- handleDefs prog
    let exp = annotateVars withDefinitions

    -- Show the result
    showProg verb exp

    -- Evaluate if requested
    when eval $ evaluate verb exp depth draws env

