{-# LANGUAGE FlexibleContexts #-}
-- Small step semantics for guarded HOPFL
module Semantics where

import Substitution
import Syntax.Abs
import Syntax.ErrM
import Syntax.Fail

import Data.HashMap.Lazy as HM
import Control.Monad.Reader
import Control.Applicative

data Value
  = VVal Double
  | VPair Value Value
  | VNext Value
  | VIn Value
  | VThunk Exp
  deriving (Eq, Show)

instance Ord Value where
  VVal m <= VVal n = m <= n
  _ <= _ = False

toExp :: Value -> Exp
toExp (VVal v) = Val v
toExp (VThunk e) = e

-- Environment as hashmap
type Env = HashMap String Exp

-- Transform the environment AST into a hashmap
mkEnv :: Environment -> Env
mkEnv (Env e) = fromList $ fmap mkAssign e
  where mkAssign (Assign (Ident x) exp) = (x, exp)

-- Update the environment
update :: String -> Exp -> Env -> Env
update = insert

pdfNorm :: (Double, Double) -> Double -> Double
pdfNorm (x, s) c = 1 / exp(((c-x) ^^ 2) / 2 * s) * sqrt(2 * s * pi)


-- Evaluation function: 
-- Takes an AST and calculates the result of the program using small
-- step semantics. Uses an error monad to allow for evaluation error logging
-- The first argument is the allowed depth for evaluating nexts. 
-- The second argument is the expression to be evaluated.
-- TODO: do not include tokens into the datatypes
-- TODO: something like LEFTFIRST?
evalExp :: MonadReader Env m => Integer -> Exp -> m Value

-- Variables
evalExp n (Var (Ident v)) = do
    val <- asks $ HM.lookup v
    case val of 
        Just e  -> evalExp n e
        Nothing -> error $ "Undefined free variable: " ++ show v

-- Values (reals)
evalExp n (Val v) = return $ VVal v

-- Later modality
evalExp n (Next e) = do
    r <- evalExp (n-1) e -- TODO: ???
    return $ VNext r

-- Put into fixpoint
evalExp n (In e) = VIn <$> evalExp n e

-- Extract from fixpoint
evalExp n (Out e) = do
    r <- evalExp n e
    case r of
        VIn v -> return v
        _ -> return $ VVal 1.0 -- TODO

-- Function application
evalExp n (App e1 e2) = do -- TODO: only allow values as arguments
    r1 <- evalExp n e1
    r2 <- evalExp n e2
    case r1 of
        VThunk e -> evalExp n (removeBinder (toExp r1) (toExp r2))
        _ -> error $ show r1 ++ " is not a function"

-- Delayed function application
evalExp n (LApp e1 _ e2) = do
    r1 <- evalExp n e1
    r2 <- evalExp n e2
    case (r1, r2) of
        (VNext (VThunk e), VNext s) -> VNext <$> evalExp n (App e $ toExp s) -- TODO: this?
        _ -> error "Invalid arguments to LApp"

-- Pair creation
evalExp n (Pair e1 e2) = VPair <$> evalExp n e1 <*> evalExp n e2

-- First projection
evalExp n (Fst e) = do
    r <- evalExp n e
    case r of
        VPair v1 v2 -> return v1
        _ -> error $ "Took fst of non-pair " ++ show r

-- Second projection
evalExp n (Snd e) = do
    r <- evalExp n e
    case r of
        VPair v1 v2 -> return v2
        _ -> error $ "Took snd of non-pair " ++ show r

-- Normal distribtion sampling
evalExp n (Norm e) = do 
    r <- evalExp n e
    case r of
        VPair (VVal m) (VVal s) -> return $ VVal $ pdfNorm (m, s) 1.0
        _ -> error "Normal argument not a pair of real numbers"

-- Function abstraction
evalExp n exp@(Abstr l x e) = return $ VThunk exp

-- Recursion
evalExp n (Rec x e) = evalExp n $ removeBinder e e

-- Typed terms
evalExp n (Typed e t) = evalExp n e

