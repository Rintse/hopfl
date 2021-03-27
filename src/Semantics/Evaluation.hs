{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Small step semantics for guarded HOPFL
module Semantics.Evaluation where

import Semantics.Substitution
import Semantics.Tools
import Syntax.IdAbs
import qualified Syntax.Abs as Raw
import Syntax.ErrM
import Semantics.Values
import Tools.Treeify
import Tools.VerbPrint

import Data.HashMap.Lazy as HM
import Data.Bifunctor
import Control.Applicative
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

-- Evaluates a program given a maximum eval depth, and environment
-- To be called from Main.hs
evaluate :: Bool -> Exp -> Integer -> [Double] -> Raw.Environment -> IO ()
evaluate v prog n s env = do
    putStrV v ("Evaluating...\n- with random draws: " ++ show s)
    putStrV v ("- using the environment: " ++ printEnv (mkEnv env))
    putStrV v ("- up to depth: " ++ show n ++ "\n")

    let toEval = evalMonad (evalExp prog)       -- Put program into eval monad
    let r1 = runReaderT toEval (mkEnv env, n)   -- Run the reader (env, depth)
    let r2 = runStateT r1 (1.0, s)              -- Run the state (density, draws)

    case runExcept r2 of                        -- Run except to catch errors
        Left s -> putStrLn $ "Evaluation failed:\n" ++ s
        Right (s, (w,_)) ->
            putStrLn $ "Result (density = " ++ show w ++ "):\n" ++ show s

-- Evaluates 2 arguments and pairs them to allow for easy pattern matching
match2 :: Exp -> Exp -> EvalMonad (Value, Value)
match2 e1 e2 = (,) <$> evalExp e1 <*> evalExp e2

-- Evaluation function: 
-- Takes an AST and calculates the result of the program using big step semantics
evalExp :: Exp -> EvalMonad Value

-- Variables
evalExp exp@(Var (Ident v _ _)) = asks (HM.lookup v . fst) >>= \case
    Just (Val v) -> return $ VVal v
    Nothing -> throwError $ "Undefined free variable: " ++ show v

-- Values (reals)
evalExp exp@(Val v) = return $ VVal v

-- Later modality: do no allow calculation past "depth" nexts
evalExp exp@(Next e) = asks snd >>= \x -> 
    if x == 0 then return $ VNext e
    else local (second $ subtract 1) (VNext . toExp <$> evalExp e)

-- Put into fixpoint
evalExp exp@(In e) = return $ VIn e

-- Extract from fixpoint
evalExp exp@(Out e) = evalExp e >>= \case
    VIn v -> evalExp v
    _ -> return $ VOut exp

-- Function application
evalExp exp@(App e1 e2) = match2 e1 e2 >>= \case
    (VThunk (Abstr l x e), r2) -> evalExp $ substitute e x $ toExp r2
    _ -> throwError $ " Application on non-function:\n" ++ treeTerm exp

-- Delayed function application
evalExp exp@(LApp e1 _ e2) = match2 e1 e2 >>= \case
    (VNext t, VNext s) -> evalExp $ Next $ App t s
    _ -> throwError $ "Invalid arguments to LApp:\n" ++ treeTerm exp

-- Pair creation
evalExp exp@(Pair e1 e2) = return $ VPair e1 e2

-- First projection
evalExp exp@(Fst e) = evalExp e >>= \case
    VPair v1 v2 -> evalExp v1;
    _ -> throwError $ "Took fst of non-pair " ++ treeTerm exp

-- Second projection
evalExp exp@(Snd e) = evalExp e >>= \case
    VPair v1 v2 -> evalExp v2
    _ -> throwError $ "Took snd of non-pair " ++ treeTerm exp

-- Normal distribtion sampling
evalExp exp@(Norm e) = evalExp e >>= \case
    VPair e1 e2 -> match2 e1 e2 >>= \case
        (VVal m, VVal v) -> performDraw m v
        _ -> throwError "Normal pair does not contain reals"
    _ -> throwError $ "Normal argument not a pair: \n" ++ treeTerm exp

-- If then else
evalExp exp@(Ite b e1 e2) = evalExp b >>= \case
    VBVal True  -> evalExp e1
    VBVal False -> evalExp e2
    _ -> throwError $ "If with non boolean condition:\n" ++ treeTerm exp

-- Coproduct injection
evalExp exp@(InL e) = return $ VInL e
evalExp exp@(InR e) = return $ VInR e

-- Matching coproducts
evalExp exp@(Match e x1 e1 x2 e2) = evalExp e >>= \case
    VInL l  -> evalExp l >>= evalExp . substitute e1 x1 . toExp
    VInR r  -> evalExp r >>= evalExp . substitute e2 x2 . toExp
    _       -> throwError $ "Match on non-coproduct:\n" ++ treeTerm exp

-- Function abstraction
evalExp exp@(Abstr l x e) = return $ VThunk exp

-- Recursion
evalExp exp@(Rec x e) = evalExp $ substitute e x $ recName (Next exp)

-- Boolean and arithmetic expressions
evalExp exp = case exp of
    BVal v      -> return $ VBVal $ toBool v
    Val v       -> return $ VVal v
    Add e1 e2   -> evalAExp e1 (+) e2
    Sub e1 e2   -> evalAExp e1 (-) e2
    Mul e1 e2   -> evalAExp e1 (*) e2
    Div e1 e2   -> evalAExp e1 (/) e2

    Not _ e     -> evalBExp1 not e
    And e1 _ e2 -> evalBExp e1 (&&) e2
    Or e1 _ e2  -> evalBExp e1 (||) e2

    Eq e1 e2    -> evalRelop e1 (==) e2
    Lt e1 e2    -> evalRelop e1 (<) e2
    Gt e1 e2    -> evalRelop e1 (>) e2
    Leq e1 _ e2 -> evalRelop e1 (<=) e2
    Geq e1 _ e2 -> evalRelop e1 (>=) e2

-- Helper functions to evaluate boolean and aritmetic expresions
-- Evaluates a binary arithmetic operation
evalAExp :: Exp -> (Double -> Double -> Double) -> Exp -> EvalMonad Value
evalAExp e1 op e2 = match2 e1 e2 >>= \case
    (VVal d1, VVal d2) -> return $ VVal $ op d1 d2
    _ -> throwError "Non-real arguments to arithmetic operator"

-- Evaluates a binary boolean operation
evalBExp :: Exp -> (Bool -> Bool -> Bool) -> Exp -> EvalMonad Value
evalBExp e1 op e2 = match2 e1 e2 >>= \case
    (VBVal b1, VBVal b2) -> return $ VBVal $ op b1 b2
    _ -> throwError "Non-bool arguments to boolean operator"

-- Evaluates a unary boolean operation
evalBExp1 :: (Bool -> Bool) -> Exp -> EvalMonad Value
evalBExp1 op e = evalExp e >>= \case
    (VBVal b) -> return $ VBVal $ op b
    _ -> throwError "Non-bool arguments to boolean"

-- Evaluates a relative operator
evalRelop :: Exp -> (Double -> Double -> Bool) -> Exp -> EvalMonad Value
evalRelop e1 op e2 = match2 e1 e2 >>= \case
    (VVal d1, VVal d2) -> return $ VBVal $ op d1 d2
    _ -> throwError "Non-real arguments to relative operator\n"

-- Performs a random draw and updates the state monad
performDraw :: Double -> Double -> EvalMonad Value
performDraw m v = do
    env <- get
    case snd env of
        (c:l) -> do
            let density = pdfNorm (m,v) c

            if isNaN density
            then throwError "PDF not defined\n"
            else modify (\(w, _) -> (w * density, l))

            return $ VVal c
        _ -> throwError $ "Draws list too small (" ++ show m ++ ", " ++ show v ++ ")"

