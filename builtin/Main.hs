module Main where

import Args
import Syntax.Parse
import qualified Syntax.IdAbs as ID
import Syntax.Raw.Abs
import Tools.Preprocess

import System.Environment ( getArgs )
import System.Console.GetOpt

defsToAsgList :: Prg -> IO [Assignment]
defsToAsgList (DefProg (Env l) e) = return $ inDefs l

defsToAsgList (Prog p) = return []

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
    defd <- defsToAsgList prog
    print $ show defd
