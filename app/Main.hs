module Main where

import Args
import Syntax.Parse
import Semantics.Evaluation
import qualified Syntax.Expression as Exp
import Syntax.Raw.Abs as Raw
import Tools.VerbPrint
import Tools.Treeify

import Preprocess.Definitions
import Preprocess.AnnotateVars
import Preprocess.Lists

import Control.Monad.Reader
import System.Environment ( getArgs )
import System.Console.GetOpt
import Control.Monad (when)
import System.Exit

-- Run all the preprocess steps
preprocess :: Raw.Prg -> IO Exp.Exp
preprocess e = do
    withDefinitions <- handleDefs e
    withLists       <- desugarLists withDefinitions
    return          $  annotateVars withLists


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

    let Options {   optVerbose  = verb,
                    optInput    = input,
                    optEval     = eval,
                    optEnv      = env,
                    optDraws    = draws,
                    optDepth    = depth     } = opts
    
    -- Parse input into a program AST
    prog <- input >>= parse verb
   
    -- Preprocess raw AST into one expression
    exp <- preprocess prog

    -- Show the result
    showProg verb exp

    -- Evaluate if requested
    when eval $ evaluate verb exp depth draws env

