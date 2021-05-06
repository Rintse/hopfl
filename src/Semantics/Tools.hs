-- Defines some helper functions for the Evaluation module 

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

module Semantics.Tools where

import Syntax.Expression
import Preprocess.AnnotateVars
import Syntax.Number
import qualified Syntax.Raw.Abs as Raw
import Syntax.Raw.ErrM
import Semantics.Values

import Data.Functor.Foldable.TH
import Data.Functor.Foldable
import Data.Functor.Foldable.Monadic
import Data.HashMap.Lazy as HM
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

-- A monad for pogram evaluation, containing:
--   - A reader with (a map from vars to their values, and the evaluation depth)
--   - A State with (a list of random draws, and the density of this execution)
--   - An error monad to express evaluation failure
newtype EvalMonad a = EvalMonad {
    evalMonad :: ReaderT (Env, Integer) (StateT (Double, [Double]) (Except String)) a
} deriving (    Functor, Applicative, Monad,
                MonadReader (Env, Integer),
                MonadState (Double, [Double]),
                MonadError String   )

-- Environment as hashmap from names to values
type Env = HashMap String Exp

-- Transform the environment AST into a hashmap
mkEnv :: Raw.Environment -> Env
mkEnv (Raw.Env e) = fromList $ fmap mkAssign e
    where mkAssign (Raw.Assign (Raw.Ident x) _ exp) = (x, annotateVars exp)

printEnv :: Env -> String
printEnv m = show $ Prelude.map 
    (\x -> fst x ++ "=" ++ case snd x of 
        Val v -> show v
    ) (toList m)

-- Evaluates 2 arguments and pairs them to allow for easy pattern matching
match2 :: (Exp -> EvalMonad Value) -> Exp -> Exp -> EvalMonad (Value, Value)
match2 f e1 e2 = (,) <$> f e1 <*> f e2

-- Probability density function of the gaussian distribution
pdfNorm :: (Double, Double) -> Double -> Double
pdfNorm (m,v) c = let sd = sqrt v in -- variance is σ^2
    (1 / (sd * sqrt (2 * pi))) * exp (-0.5 * (((c - m) / sd) ^^ 2)) 

printDist :: Double -> Double -> String
printDist m v = "Normal (mean = " ++ show m ++ ", variance = " ++ show v ++ ")"

-- Performs a random draw and updates the state monad
performDraw :: Double -> Double -> EvalMonad Value
performDraw m v = gets snd >>= \case
    (c:rest) -> let d = pdfNorm (m,v) c in
        if  | isNaN d -> throwError $
                "PDF of " ++ printDist m v ++ " not defined for value " ++ show d

            | (==0) d -> throwError $ 
                "Impossible draw for " ++ printDist m v ++ ": " ++ show d

            | otherwise -> do
                modify (\(w, _) -> (w * d, rest))
                return $ VVal $ Fract c
    _ -> throwError "Draws list too small"

-- Helper functions to evaluate boolean and aritmetic expresions
-- Evaluates a binary arithmetic operation
evalAExp :: (Exp -> EvalMonad Value) -> Exp 
    -> (Number -> Number -> Number) -> Exp -> EvalMonad Value
evalAExp f e1 op e2 = match2 f e1 e2 >>= \case
    (VVal v1, VVal v2) -> return $ VVal $ op v1 v2
    other -> throwError $ "Non-real args to arithmetic operator:\n" ++ show other

evalAExp1 :: (Exp -> EvalMonad Value) -> (Number -> Number) -> Exp -> EvalMonad Value
evalAExp1 f op e = f e >>= \case
    (VVal v) -> return $ VVal $ op v
    other -> throwError $ "Non-real arg to arithmetic operator:\n" ++ show other

-- -- Evaluates a binary boolean operation
evalBExp :: (Exp -> EvalMonad Value) -> Exp 
         -> (Bool -> Bool -> Bool) -> Exp -> EvalMonad Value
evalBExp f e1 op e2 = match2 f e1 e2 >>= \case
    (VBVal b1, VBVal b2) -> return $ VBVal $ op b1 b2
    other -> throwError $ "Non-bool args to bool operator:\n" ++ show other

-- -- Evaluates a unary boolean operation
evalBExp1 :: (Exp -> EvalMonad Value) 
          -> (Bool -> Bool) -> Exp -> EvalMonad Value
evalBExp1 f op e = f e >>= \case
    VBVal b -> return $ VBVal $ op b
    other -> throwError $ "Non-bool args to bool operator:\n" ++ show other

-- -- Evaluates a relative operator
evalRelop :: (Exp -> EvalMonad Value) -> Exp 
          -> (Number -> Number -> Bool) -> Exp -> EvalMonad Value
evalRelop f e1 op e2 = match2 f e1 e2 >>= \case
    (VVal v1, VVal v2) -> return $ VBVal $ op v1 v2
    other -> throwError $ "Non-real args to relative operator:\n" ++ show other

-- Evluates everything underneath values to make it readable
forceEval :: (Exp -> EvalMonad Value ) -> Value -> EvalMonad Value
forceEval f = anaM $ \case
    VPair e1 e2 -> EPairF <$> f e1 <*> f e2
    VInL e -> EInLF <$> f e
    VInR e -> EInRF <$> f e
    other -> return $ project other

