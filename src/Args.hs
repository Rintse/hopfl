-- Module that defines the arguments to the program as getOpt
-- data types such that parameters can be easily parsed in Main

module Args where

import Syntax.Raw.Abs
import Syntax.Parse

import System.Console.GetOpt
import Control.Exception
import System.IO as IO 
import System.Exit
import System.Environment
import Data.Char
import Data.List.Split
import Text.Read (readMaybe)
import Data.Maybe

-- The option list
data Options = Options
    { optVerbose    :: Bool
    , optInput      :: IO String
    , optEval       :: Bool 
    , optEnv        :: Environment 
    , optDraws      :: [Double]
    , optDepth      :: Integer }

-- The default options
defaultOpts :: Options
defaultOpts = Options  
    { optVerbose    = False
    , optInput      = getContents
    , optEval       = False 
    , optEnv        = Env [] 
    , optDraws      = [] 
    , optDepth      = 0 }

-- Reads a file if such an argument is given
readFile :: String -> Options -> IO Options
readFile arg opt = do
    file <- try (IO.readFile arg) :: ParseMonad String
    case file of
        Left ex -> do 
            putStrLn $ "Error opening file:\n" ++ show ex
            exitFailure
        Right content -> return opt { optInput = return content }

readEnv :: String -> Options -> IO Options
readEnv arg opt = do
    env <- try (parseEnv arg) :: ParseMonad Environment
    case env of
        Left _ -> do 
            putStrLn $ "Error parsing environment: " ++ arg
            exitFailure
        Right val -> return opt { optEnv = val }

-- Parses the depth
parseDepth :: String -> IO Integer
parseDepth s = case readMaybe s of 
    Nothing -> throw DepthException
    Just n  -> return n

readDepth :: String -> Options -> IO Options
readDepth arg opt = do
    depth <- try (parseDepth arg) :: ParseMonad Integer
    case depth of 
        Left _ -> do 
            putStrLn $ "Error parsing depth: " ++ arg
            exitFailure
        Right n  -> return opt { optDepth = n }

-- Parses the random draws if such an argument is given
parseDraws :: String -> IO [Double]
parseDraws s = let l = filter (not . all isSpace) $ splitOn ";" s 
    in let parsed = map (readMaybe :: String -> Maybe Double) l
    in if Nothing `elem` parsed 
    then throw DrawListException
    else return $ catMaybes parsed

readDraws :: String -> Options -> IO Options
readDraws arg opt = do
    draws <- try (parseDraws arg) :: ParseMonad [Double]
    case draws of
        Left _ -> do
            putStrLn $ "Error parsing random draws: " ++ arg
            exitFailure
        Right val -> return opt { optDraws = val }

-- Sets the verbosity
readVerb :: Options -> IO Options
readVerb opt = return opt { optVerbose = True }

-- Sets evaluation
readEval :: Options -> IO Options
readEval opt = return opt { optEval = True }

-- Outputs a help message
putHelp :: Options -> IO Options
putHelp opt = do
    prg <- getProgName
    hPutStrLn stderr (usageInfo prg options)
    exitSuccess

-- The options as functions to be threaded through
options :: [ OptDescr (Options -> IO Options) ]
options = 
    [ Option "i" ["input"] (ReqArg Args.readFile "FILE") 
        "Input file"

    , Option "E" ["evaluate"] (NoArg readEval) 
        "Evaluate program using big step semantics"

    , Option "e" ["environment"] (ReqArg readEnv "\"x=1.0;y=2.0..\"")
        "Environment to evaluate within"

    , Option "n" ["nexts"] (ReqArg readDepth "INT")
        "Maximum number of next terms to evaluate into"

    , Option "d" ["draws"] (ReqArg readDraws "\"1.0;2.0..\"")
        "A list of random draws to use when sampling distributions"
    
    , Option "v" ["verbose"] (NoArg readVerb) 
        "Enable verbose parsing"

    , Option "h" ["help"] (NoArg putHelp) 
        "Display help message" 
    ]

