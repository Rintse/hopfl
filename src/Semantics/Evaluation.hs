-- Defines a evaluation function for expressions
-- which implements a call-by-value big-step semantics 

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

-- Evaluates a program given a maximum eval depth, and environment
-- To be called from Main.hs
evaluate :: Bool -> Exp -> Integer -> [Double] -> Raw.Environment -> IO ()
evaluate v prog n s env = do
    putStrV v ("Evaluating...\n- with random draws: " ++ show s)
    putStrV v ("- using the environment: " ++ printEnv (mkEnv env))
    putStrV v ("- up to depth: " ++ show n ++ "\n")

    let toEval = evalMonad (eval prog)       -- Put program into eval monad
    let r1 = runReaderT toEval (mkEnv env, n)   -- Run the reader (env, depth)
    let r2 = runStateT r1 (1.0, s)              -- Run the state (density, draws)

    case runExcept r2 of                        -- Run except to catch errors
        Left s -> putStrLn $ "Evaluation failed:\n" ++ s
        Right (s, (w,_)) ->
            putStrLn $ "Result (density = " ++ show w ++ "):\n" ++ show s

-- Evaluation function: 
-- Takes an AST and calculates the result of the program using big step semantics
eval :: Exp -> EvalMonad Value

-- Variables
eval exp@(Var (Ident v _ _)) = asks (HM.lookup v . fst) >>= \case
    Just (Val v) -> return $ VVal v
    Nothing -> throwError $ "Undefined free variable: " ++ show v

-- Values (reals)
eval exp@(Val v) = return $ VVal v

-- Later modality: do no allow calculation past "depth" nexts
eval exp@(Next e) = asks snd >>= \x -> 
    if x == 0 then return $ VNext e
    else local (second $ subtract 1) (VNext . toExp <$> eval e)

-- Put into fixpoint
eval exp@(In e) = return $ VIn e

-- Extract from fixpoint
eval exp@(Out e) = eval e >>= \case
    VIn v -> eval v
    _ -> return $ VOut exp

-- Function application
eval exp@(App e1 e2) = match2 eval e1 e2 >>= \case
    (VThunk (Abstr l x e), r2) -> eval $ substitute e x $ toExp r2
    _ -> throwError $ " Application on non-function:\n" ++ treeTerm exp

-- Delayed function application
eval exp@(LApp e1 _ e2) = match2 eval e1 e2 >>= \case
    (VNext t, VNext s) -> eval $ Next $ App t s
    _ -> throwError $ "Invalid arguments to LApp:\n" ++ treeTerm exp

-- Pair creation
eval exp@(Pair e1 e2) = return $ VPair e1 e2

-- First projection
eval exp@(Fst e) = eval e >>= \case
    VPair v1 v2 -> eval v1;
    _ -> throwError $ "Took fst of non-pair " ++ treeTerm exp

-- Second projection
eval exp@(Snd e) = eval e >>= \case
    VPair v1 v2 -> eval v2
    _ -> throwError $ "Took snd of non-pair " ++ treeTerm exp

-- Normal distribtion sampling
eval exp@(Norm e) = eval e >>= \case
    VPair e1 e2 -> match2 eval e1 e2 >>= \case
        (VVal m, VVal v) -> performDraw m v
        _ -> throwError "Normal pair does not contain reals"
    _ -> throwError $ "Normal argument not a pair: \n" ++ treeTerm exp

-- If then else
eval exp@(Ite b e1 e2) = eval b >>= \case
    VBVal True  -> eval e1
    VBVal False -> eval e2
    _ -> throwError $ "If with non boolean condition:\n" ++ treeTerm exp

-- Coproduct injection
eval exp@(InL e) = return $ VInL e
eval exp@(InR e) = return $ VInR e

-- Matching coproducts
eval exp@(Match e x1 e1 x2 e2) = eval e >>= \case
    VInL l  -> eval l >>= eval . substitute e1 x1 . toExp
    VInR r  -> eval r >>= eval . substitute e2 x2 . toExp
    _       -> throwError $ "Match on non-coproduct:\n" ++ treeTerm exp

-- Function abstraction
eval exp@(Abstr l x e) = return $ VThunk exp

-- Recursion
eval exp@(Rec x e) = eval $ substitute e x $ recName (Next exp)

-- Boolean and arithmetic expressions
eval exp = case exp of
    BVal v      -> return $ VBVal $ toBool v
    Val v       -> return $ VVal v
    Add e1 e2   -> evalAExp eval e1 (+) e2
    Sub e1 e2   -> evalAExp eval e1 (-) e2
    Mul e1 e2   -> evalAExp eval e1 (*) e2
    Div e1 e2   -> evalAExp eval e1 (/) e2

    Not _ e     -> evalBExp1 eval not e
    And e1 _ e2 -> evalBExp eval e1 (&&) e2
    Or e1 _ e2  -> evalBExp eval e1 (||) e2

    Eq e1 e2    -> evalRelop eval e1 (==) e2
    Lt e1 e2    -> evalRelop eval e1 (<) e2
    Gt e1 e2    -> evalRelop eval e1 (>) e2
    Leq e1 _ e2 -> evalRelop eval e1 (<=) e2
    Geq e1 _ e2 -> evalRelop eval e1 (>=) e2

