module Main where

import Syntax.IdAbs
import Semantics.Evaluation
import Tools.VerbPrint
import Tools.Treeify
import Tools.Preprocess
import Args
import Syntax.Parse

import Control.Monad.Reader
import System.Environment ( getArgs )
import System.Console.GetOpt
import Control.Monad (when)
import System.Exit

-- Parses the arguments, input and performs the requested actions
main :: IO ()
main = do
    args <- getArgs -- Get and parse options
    let (optArgs, nonOpts, errs) = getOpt RequireOrder Args.options args

    -- Errors parsing arguments
    unless (null errs) ( do
        putStrLn "Errors parsing arguments:"
        mapM_ putStr errs
        exitFailure )

    opts <- foldl (>>=) (return defaultOpts) optArgs

    let Options {   optVerbose  = verb,
                    optInput    = input,
                    optEval     = eval,
                    optEnv      = env,
                    optDraws    = draws,
                    optDepth    = depth     } = opts

    -- Parse input
    prog <- input >>= parse verb

    -- Preprocess definitions
    let exp = handleDefs prog

    -- Annotate identifiers with a unique id
    let annotated = idExp exp
    showProg verb annotated

    -- Evaluate if requested
    when eval $ evaluate verb annotated depth draws env

