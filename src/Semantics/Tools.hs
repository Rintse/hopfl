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
import Semantics.Sampling

import Control.Exception.Base
import Data.Functor.Foldable.TH
import Data.Functor.Foldable
import Data.Functor.Foldable.Monadic
import Data.HashMap.Lazy as HM
import Data.Bifunctor
import Debug.Trace
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
    where mkAssign (Raw.Assign (Raw.Ident x) _ exp) =
            (x, annotateVars exp)

printEnv :: Env -> String
printEnv m = show $ Prelude.map
    (\x -> fst x ++ "=" ++ show (snd x)) (toList m)

-- Evaluates 2 arguments and pairs them to allow for easy pattern matching
match2 :: (Exp -> EvalMonad Value) -> Exp -> Exp -> EvalMonad (Value, Value)
match2 f e1 e2 = (,) <$> f e1 <*> f e2

match3 :: (Exp -> EvalMonad Value) -> Exp -> Exp -> Exp -> EvalMonad (Value, Value, Value)
match3 f e1 e2 e3 = (,,) <$> f e1 <*> f e2 <*> f e3

-- Performs a random draw and updates the state monad
performDraw :: Distribution -> [Double] -> EvalMonad Value
performDraw dist params = gets snd >>= \case
    (c:rest) -> let d = pdf dist params c in
        if  | isNaN d -> throwError $
                "PDF " ++ show (name dist) ++ " not defined for value " ++ show d

            | (==0) d -> throwError $
                "Impossible draw for " ++ show (name dist )
                ++ " with parameters" ++ show params ++ ": " ++ show d

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

evalAExp1 :: (Exp -> EvalMonad Value) -> (Number
          -> Number) -> Exp -> EvalMonad Value
evalAExp1 f op e = f e >>= \case
    (VVal v) -> return $ VVal $ op v
    other -> throwError $ "Non-real arg to arithmetic operator:\n" ++ show other

-- Evaluates a binary boolean operation
evalBExp :: (Exp -> EvalMonad Value) -> Exp
         -> (Bool -> Bool -> Bool) -> Exp -> EvalMonad Value
evalBExp f e1 op e2 = match2 f e1 e2 >>= \case
    (VBVal b1, VBVal b2) -> return $ VBVal $ op b1 b2
    other -> throwError $ "Non-bool args to bool operator:\n" ++ show other

-- Evaluates a unary boolean operation
evalBExp1 :: (Exp -> EvalMonad Value)
          -> (Bool -> Bool) -> Exp -> EvalMonad Value
evalBExp1 f op e = f e >>= \case
    VBVal b -> return $ VBVal $ op b
    other -> throwError $ "Non-bool args to bool operator:\n" ++ show other

-- Evaluates a relative operator
evalRelop :: (Exp -> EvalMonad Value) -> Exp
          -> (Number -> Number -> Bool) -> Exp -> EvalMonad Value
evalRelop f e1 op e2 = match2 f e1 e2 >>= \case
    (VVal v1, VVal v2) -> return $ VBVal $ op v1 v2
    other -> throwError $ "Non-real args to relative operator:\n" ++ show other

-- Evluates everything underneath certain values to make it readable
forceEval :: (Exp -> EvalMonad Value) -> Value -> EvalMonad Value
forceEval f = \case
    -- Still refuse to go underneath too many nexts
    VNext e     -> asks snd >>= \x ->
        if x == 0 then return $ VUNext e
        else local (second $ subtract 1) ( VENext <$> (f e >>= forceEval f) )

    VIn e       -> VEIn     <$> (f e >>= forceEval f)
    VInL e      -> VEInL    <$> (f e >>= forceEval f)
    VInR e      -> VEInR    <$> (f e >>= forceEval f)
    VBox l e    -> VEBox    <$> (f e >>= forceEval f)
    VPair e1 e2 -> VEPair   <$> (f e1 >>= forceEval f) 
                            <*> (f e2 >>= forceEval f)
    other       -> return other

