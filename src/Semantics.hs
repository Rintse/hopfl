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
debugRet :: Exp -> Value -> SemEnv Value
debugRet e v = trace (show e ++ "\nReturning: " ++ show v ++ "\n") return v

evalExp :: Exp -> SemEnv Value

-- Variables
evalExp exp@(Var (Ident v)) = do
    val <- asks (\(x,_) -> HM.lookup v x)
    case val of
        Just e  -> do
            r <- evalExp e
            debugRet exp r
        Nothing -> error $ "Undefined free variable: " ++ show v

-- Values (reals)
evalExp exp@(Val v) = debugRet exp (VVal v)

-- Later modality
-- Do no allow calculation past n (param) nexts
evalExp exp@(Next e) = do
    depth <- asks snd
    if depth == 0 then 
        trace "NUL" 
        (debugRet exp $ VNext $ VThunk e)
    else 
        trace "-1" 
        (local (second $ subtract 1) ( do
            r <- evalExp e
            debugRet exp $ VNext r 
        ))

-- Put into fixpoint
evalExp exp@(In e) = do 
    r <- evalExp e
    debugRet exp $ VIn r

-- Extract from fixpoint
evalExp exp@(Out e) = do
    r <- evalExp e
    case r of
        VIn v -> debugRet exp v
        _ -> debugRet exp $ VThunk exp

-- Function application
evalExp exp@(App e1 e2) = do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case r1 of
        VThunk Abstr {} -> do
            r <- evalExp (removeBinder (toExp r1) (toExp r2))
            debugRet exp r
        _ -> error $ show r1 ++ " is not a function"

-- Delayed function application
evalExp exp@(LApp e1 _ e2) = do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
        (VNext e, VNext s) -> do
            r <- evalExp $ Next (App (toExp e) (toExp s)) -- TODO: this?
            debugRet exp r
        _ -> error "Invalid arguments to LApp"

-- Pair creation
evalExp exp@(Pair e1 e2) = do 
    r1 <- evalExp e1 
    r2 <- evalExp e2 
    debugRet exp $ VPair r1 r2

-- First projection
evalExp exp@(Fst e) = do
    r <- evalExp e
    case r of
        VPair v1 v2 -> debugRet exp v1
        _ -> error $ "Took fst of non-pair " ++ show r

-- Second projection
evalExp exp@(Snd e) = do
    r <- evalExp e
    case r of
        VPair v1 v2 -> debugRet exp v2
        _ -> error $ "Took snd of non-pair " ++ show r

-- Normal distribtion sampling
evalExp exp@(Norm e) = do
    r <- evalExp e
    case r of
        VPair (VVal m) (VVal s) -> do
            (w, l) <- get
            case l of
                (c:_)   -> do
                    modify (\(w, l) -> (w * pdfNorm (m,s) c, tail l))
                    debugRet exp $ VVal c
                _       -> error "Random draws list too small"
        _ -> error "Normal argument not a pair of real numbers"

-- Function abstraction
evalExp exp@(Abstr l x e) = debugRet exp $ VThunk exp

-- Recursion
evalExp exp@(Rec x e) = do 
    r <- evalExp $ removeBinder exp (Next exp)
    debugRet exp r

-- Typed terms
evalExp (Typed e t) = evalExp e

