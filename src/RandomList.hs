module RandomList where

import Control.Monad.State
import Data.Random.Normal
import Control.Monad.Random
import System.Random

import Syntax.Abs

-- TODO init seed based on something like time?
-- Builds a sequence of random numbers that correspond with each 
-- of the normal calls present in the program. Actually generates 
-- numbers from a normal distribution that matches the normal call, 
-- to avoid having very small densities. 

genList :: Exp -> [Double]
genList e = execState (runRandT (subList e) (mkStdGen 200)) []

-- TODO check for faulty programs
subList :: Exp -> RandT StdGen (State [Double]) Exp
subList exp = case exp of
    Var (Ident v)   -> return exp
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
