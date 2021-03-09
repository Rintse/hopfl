{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RandomList where

import Syntax.Abs as S

import Control.Monad.State
import Data.Random.Normal
import Control.Monad.Random
import Data.List.Split
import Data.Maybe
import System.Random
import Text.Read
import Syntax.ErrM


-- Parses a user-provided list
parseList :: String -> Err [Double]
parseList s = let l = map (readMaybe :: String -> Maybe Double) (splitOn "," s)
    in if Nothing `elem` l then Bad []
    else Ok (catMaybes l)


-- TODO init seed based on something like time?
-- Builds a sequence of random numbers that correspond with each 
-- of the normal calls present in the program. 
genList :: Exp -> [Double]
genList e = execState (runRandT (subList e) (mkStdGen 3)) []

-- TODO check for faulty programs
subList :: Exp -> RandT StdGen (State [Double]) Exp
subList exp = case exp of
    Var (S.Ident v) -> return exp
    Val v           -> return exp
    Next e          -> Next <$> subList e
    In e            -> In <$> subList e
    Out e           -> Out <$> subList e
    App e1 e2       -> liftM2 App (subList e1) (subList e2)
    LApp e1 o e2    -> liftM3 LApp (subList e1) (return o) (subList e2)
    Pair e1 e2      -> liftM2 Pair (subList e1) (subList e2)
    Fst e           -> Fst <$> subList e
    Snd e           -> Snd <$> subList e
    Abstr l v e     -> Abstr l v <$> subList e
    Rec v e         -> Rec v <$> subList e
    Typed e t       -> liftM2 Typed (subList e) (return t)
    Norm e          -> do
        r <- getRandom
        modify ([r] ++)
        return exp
