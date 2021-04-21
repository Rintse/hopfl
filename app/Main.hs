module Main where

import Syntax.Par
import Syntax.IdAbs
import qualified Syntax.Abs as Raw
import Syntax.ErrM
import Semantics.Evaluation
import Tools.VerbPrint
import Tools.Treeify
import Args

import System.Environment ( getArgs )
import System.Console.GetOpt
import Control.Monad (when)
import System.Exit

import Control.Monad.Reader

-- Parses contents of given input file
parse :: Bool -> String -> IO Raw.Exp
parse v s = do
    putStrV v "Parsing program"
    let ts = myLLexer s
    case pExp ts of
        Bad r -> do
            putStrLn $"Parse failed: " ++ r
            putStrV v $ "Tokens still in stream:\n" ++ show ts
            exitFailure
        Ok r -> do
            putStrV v "Parse successful"
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
    prog <- input >>= parse verb

    -- Annotate identifiers with a unique id
    let annotated = idExp prog
    showProg verb annotated

    -- Evaluate if requested
    when eval $ evaluate verb annotated depth draws env
