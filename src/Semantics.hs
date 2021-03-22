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

-- Result values
data Value
  = VVal Double
  | VBVal Bool
  | VPair Value Value
  | VIn Value
  | VInL Value
  | VInR Value
  | VNext Exp
  | VOut Exp
  | VThunk Exp
  deriving (Eq, Show)

fromBool :: Bool -> BConst
fromBool b = if b then BTrue else BFalse
toBool :: BConst -> Bool
toBool b = case b of
    BTrue -> True
    BFalse -> False

toExp :: Value -> Exp
toExp (VVal v) = Val v
toExp (VBVal v) = BVal (fromBool v)
toExp (VIn e) = In $ toExp e
toExp (VInL e) = InL $ toExp e
toExp (VInR e) = InR $ toExp e
toExp (VThunk e) = e
toExp (VPair e1 e2) = Pair (toExp e1) (toExp e2)
toExp (VNext e) = Next e
toExp (VOut e) = Out e

-- Environment as hashmap
type Env = HashMap String Exp

-- Transform the environment AST into a hashmap
mkEnv :: Environment -> Env
mkEnv (Env e) = fromList $ fmap mkAssign e
    where mkAssign (Assign (Ident x) exp) = (x, exp)

-- Probability density function of the gaussian distribution
pdfNorm :: (Double, Double) -> Double -> Double
pdfNorm (m,v) c = let sd = sqrt v -- variance is σ^2
    in let density = (1 / (sd * sqrt (2 * pi))) * exp (-0.5 * (((c - m) / sd) ^^ 2)) in
    trace ("Random draw " ++ show c ++ " has density " ++ show density ++
           " for the guassian (" ++ show m ++ "," ++ show v ++ ")") density


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
evalExp exp@(Var (Ident v)) = do
    val <- asks (\(x,_) -> HM.lookup v x)
    case val of
        Just e  -> evalExp e
        Nothing -> error $ "Undefined free variable: " ++ show v

-- Values (reals)
evalExp exp@(Val v) = return $ VVal v

-- Later modality
-- Do no allow calculation past n (param) nexts
evalExp exp@(Next e) = do
    depth <- asks snd
    if depth == 0 then return $ VNext e
    else local (second $ subtract 1) (VNext <$> (toExp <$> evalExp e))

-- Put into fixpoint
evalExp exp@(In e) = VIn <$> evalExp e

-- Extract from fixpoint
evalExp exp@(Out e) = do
    r <- evalExp e
    case r of
        VIn v -> return v
        _ -> return $ VOut exp

-- Function application
evalExp exp@(App e1 e2) = do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case r1 of
        VThunk Abstr {} -> evalExp $ removeBinder (toExp r1) (toExp r2)
        _ -> error $ show r1 ++ " is not a function"

-- Delayed function application
evalExp exp@(LApp e1 _ e2) = do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
        (VNext e, VNext s) -> evalExp $ Next $ App e s -- TODO: this?
        _ -> error "Invalid arguments to LApp"

-- Pair creation
evalExp exp@(Pair e1 e2) = VPair <$> evalExp e1 <*> evalExp e2

-- First projection
evalExp exp@(Fst e) = do
    r <- evalExp e
    case r of
        VPair v1 v2 -> return v1
        _ -> error $ "Took fst of non-pair " ++ show r

-- Second projection
evalExp exp@(Snd e) = do
    r <- evalExp e
    case r of
        VPair v1 v2 -> return v2
        _ -> error $ "Took snd of non-pair " ++ show r

-- Normal distribtion sampling
evalExp exp@(Norm e) = do
    r <- evalExp e
    case r of
        VPair (VVal m) (VVal v) -> do
            l <- gets snd
            case l of
                (c:l2) -> do -- Todo: rename l2
                    let density = pdfNorm (m,v) c in
                        if isNaN density
                        then error $ "PDF not defined:\n" ++ show exp
                        else modify (\(w, l) -> (w * density, l2))
                    return $ VVal c
                _ -> error "Random draws list too small"
        _ -> error $ "Normal argument not a pair of reals:\n" ++ show exp

-- If then else
evalExp exp@(Ite b e1 e2) = do
    rb <- evalExp b
    case rb of
        VBVal True  -> evalExp e1
        VBVal False -> evalExp e2
        _ -> error $ "If with non boolean condition:\n" ++ show exp

-- Coproduct injection
evalExp exp@(InL e) = VInL <$> evalExp e
evalExp exp@(InR e) = VInR <$> evalExp e

-- Case of ...
evalExp exp@(Match e (Ident x1) e1 (Ident x2) e2) = do
    re <- evalExp e
    case re of
        VInL l -> do
            r1 <- evalExp $ toExp l
            evalExp $ runReader (subst e1) (x1, toExp r1)
        VInR r -> do
            r2 <- evalExp $ toExp r
            evalExp $ runReader (subst e2) (x2, toExp r2)
        _       -> error $ "Match on non-coproduct:\n" ++ show exp

-- Function abstraction
evalExp exp@(Abstr l x e) = return $ VThunk exp

-- Recursion
evalExp exp@(Rec x e) = evalExp $ removeBinder exp (Next exp)

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


-- HELPERS
-- Evaluates a binary arithmetic operation
evalAExp :: Exp -> (Double -> Double -> Double) -> Exp -> SemEnv Value
evalAExp e1 op e2 = do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
        (VVal d1, VVal d2) -> return $ VVal $ op d1 d2
        _ -> error "Non-real arguments to arithmetic operator"

-- Evaluates a binary boolean operation
evalBExp :: Exp -> (Bool -> Bool -> Bool) -> Exp -> SemEnv Value
evalBExp e1 op e2 = do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
        (VBVal b1, VBVal b2) -> return $ VBVal $ op b1 b2
        _ -> error "Non-real arguments to arithmetic operator"

-- Evaluates a unary boolean operation
evalBExp1 :: (Bool -> Bool) -> Exp -> SemEnv Value
evalBExp1 op e = do
    r <- evalExp e
    case r of
        (VBVal b) -> return $ VBVal $ op b
        _ -> error "Non real arguments to arithmetic operator"

-- Evaluates a relative operator
evalRelop :: Exp -> (Double -> Double -> Bool) -> Exp -> SemEnv Value
evalRelop e1 op e2 = do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
        (VVal d1, VVal d2) -> return $ VBVal $ op d1 d2
        _ -> error "Non real arguments to relative operator"

