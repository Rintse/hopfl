{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NonDet where

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

buildSeq :: Exp -> [Double]
buildSeq e = execState (runStateT (subSeq e) (mkStdGen 200)) []

-- TODO check for faulty programs
subSeq :: Exp -> StateT StdGen (State [Double]) Exp
subSeq exp = case exp of
    Var (Ident v)   -> return exp
    Val v           -> return exp
    Next e          -> Next <$> subSeq e
    In e            -> In <$> subSeq e
    Out e           -> Out <$> subSeq e
    App e1 e2       -> liftM2 App (subSeq e1) (subSeq e2)
    LApp e1 o e2    -> liftM3 LApp (subSeq e1) (return o) (subSeq e2)
    Pair e1 e2      -> liftM2 Pair (subSeq e1) (subSeq e2)
    Fst e           -> Fst <$> subSeq e
    Snd e           -> Snd <$> subSeq e
    Abstr l v e     -> Abstr l v <$> subSeq e
    Rec v e         -> Rec v <$> subSeq e
    Typed e t       -> liftM2 Typed (subSeq e) (return t)
    Norm (Pair (Val v1) (Val v2)) -> do
        r <- gets (normal' (v1, v2))
        modify (\x -> snd r)
        lift (modify ([fst r] ++)) >> return exp
