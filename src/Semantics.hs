{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Small step semantics for guarded HOPFL
module Semantics where

import Substitution
import Syntax.Abs
import Syntax.ErrM
import Syntax.Fail

import Data.HashMap.Lazy as HM
import Data.Bifunctor
import Debug.Trace
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative

-- Result values
data Value
  = VVal Double
  | VPair Value Value
  | VNext Value
  | VIn Value
  | VThunk Exp
  deriving (Eq, Show)

toExp :: Value -> Exp
toExp (VVal v) = Val v
toExp (VThunk e) = e


-- Environment as hashmap
type Env = HashMap String Exp

-- Transform the environment AST into a hashmap
mkEnv :: Environment -> Env
mkEnv (Env e) = fromList $ fmap mkAssign e
  where mkAssign (Assign (Ident x) exp) = (x, exp)


-- Probability density function of the gaussian distribution
pdfNorm :: (Double, Double) -> Double -> Double
pdfNorm (x, s) c = 1 / exp(((c-x) ^^ 2) / 2 * s) * sqrt(2 * s * pi)


-- A monad containing:
--   - A reader with (a map from vars to their values, and the evaluation depth)
--   - A State with (a list of random draws, and the density of this execution)
--   - An error monad to express evaluation failure
newtype SemEnv a = SemEnv
  { runSem :: ReaderT (Env, Integer) (StateT (Double, [Double]) Err) a
  } deriving (  Functor, Applicative, Monad,
                MonadState (Double, [Double]),
                MonadReader (Env, Integer))

-- Evaluation function: 
-- Takes an AST and calculates the result of the program using small
-- step semantics. Uses an error monad to allow for evaluation error logging
-- The first argument is the allowed depth for evaluating nexts. 
-- The second argument is the expression to be evaluated.
-- TODO: do not include tokens into the datatypes
-- TODO: something like LEFTFIRST?
evalExp :: Exp -> SemEnv Value

-- Variables
evalExp (Var (Ident v)) = do
    val <- asks (\(x,_) -> HM.lookup v x)
    case val of
        Just e  -> evalExp e
        Nothing -> error $ "Undefined free variable: " ++ show v

-- Values (reals)
evalExp (Val v) = return $ VVal v

-- Later modality
-- Do no allow calculation past n (param) nexts
evalExp exp@(Next e) = do
    depth <- asks snd
    if depth == 0 then trace "NUL" (return $ VThunk exp)
    else local (second $ subtract 1) (VNext <$> evalExp e)

-- Put into fixpoint
evalExp (In e) = VIn <$> evalExp e

-- Extract from fixpoint
evalExp (Out e) = do
    r <- evalExp e
    case r of
        VIn v -> return v
        _ -> return $ VVal 1.0 -- TODO

-- Function application
evalExp (App e1 e2) = do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case r1 of
        VThunk Abstr {} -> evalExp (removeBinder (toExp r1) (toExp r2))
        _ -> error $ show r1 ++ " is not a function"

-- Delayed function application
evalExp (LApp e1 _ e2) = do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
        (VNext (VThunk e), VNext s) -> VNext <$> evalExp (App e $ toExp s) -- TODO: this?
        _ -> error "Invalid arguments to LApp"

-- Pair creation
evalExp (Pair e1 e2) = VPair <$> evalExp e1 <*> evalExp e2

-- First projection
evalExp (Fst e) = do
    r <- evalExp e
    case r of
        VPair v1 v2 -> return v1
        _ -> error $ "Took fst of non-pair " ++ show r

-- Second projection
evalExp (Snd e) = do
    r <- evalExp e
    case r of
        VPair v1 v2 -> return v2
        _ -> error $ "Took snd of non-pair " ++ show r

-- Normal distribtion sampling
evalExp (Norm e) = do
    r <- evalExp e
    case r of
        VPair (VVal m) (VVal s) -> do
            (w, l) <- get
            case l of
                (c:_)   -> do
                    modify (\(w, l) -> (w * pdfNorm (m,s) c, tail l))
                    return $ VVal c
                _       -> error "Random draws list too small"
        _ -> error "Normal argument not a pair of real numbers"

-- Function abstraction
evalExp exp@(Abstr l x e) = return $ VThunk exp

-- Recursion
evalExp (Rec x e) = evalExp $ removeBinder e e

-- Typed terms
evalExp (Typed e t) = evalExp e

