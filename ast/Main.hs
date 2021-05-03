module Main where

import Args
import Syntax.Parse
import Syntax.IdAbs
import Tools.Preprocess

import System.Environment ( getArgs )
import System.Console.GetOpt

main :: IO ()
main = do
    args <- getArgs
    let (optArgs, nonOpts, errs) = getOpt RequireOrder Args.options args

    opts <- foldl (>>=) (return defaultOpts) optArgs

    let Options {   optVerbose  = verb,
                    optInput    = input,
                    optEval     = eval,
                    optEnv      = env,
                    optDraws    = draws,
                    optDepth    = depth     } = opts
    
    prog <- input >>= parse False
    defd <- handleDefs prog
    print $ show defd
