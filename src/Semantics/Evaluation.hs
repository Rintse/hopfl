-- Defines a evaluation function for expressions
-- which implements a call-by-value big-step semantics 

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Small step semantics for guarded HOPFL
module Semantics.Evaluation where

import Semantics.Substitution
import Semantics.Tools
import Syntax.Expression
import qualified Syntax.Raw.Abs as Raw
import Syntax.Number
import Semantics.Values
import Semantics.Sampling
import Tools.Treeify
import Tools.VerbPrint

import Data.Tree
import Data.Functor.Foldable.TH
import Data.Functor.Foldable
import Data.Functor.Foldable.Monadic
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

    let toEval = evalMonad (eval prog)          -- Put program into eval monad
    let r1 = runReaderT toEval (mkEnv env, n)   -- Run the reader (env, depth)
    let r2 = runStateT r1 (1.0, s)              -- Run the state (density, draws)
    let r3 = runExcept r2                       -- Run except to catch errors

    case r3 of
        Left s -> putStrLn $ "Evaluation failed:\n" ++ s
        Right (s, (w,d)) ->
            putStrLn $ "Result (density = "
                ++ show w ++ ", remainings draws = "
                ++ show d ++ "):\n"
                ++ treeValue s

-- Evaluation function: 
-- Takes an AST and calculates the result of the program using big step semantics
eval :: Exp -> EvalMonad Value

-- Variables
eval exp@(Var (Ident v _ _)) = asks (HM.lookup v . fst) >>= \case
    Just v -> eval v
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
    _ -> throwError $ " Out on non-In:\n" ++ treeTerm exp

-- Function application
eval exp@(App e1 e2) = match2 eval e1 e2 >>= \case
    (VThunk (Abstr x e), r2) -> eval $ substitute e x $ toExp r2
    _ -> throwError $ " Application on non-function:\n" ++ treeTerm exp

-- Delayed function application
eval exp@(LApp e1 e2) = match2 eval e1 e2 >>= \case
    (VNext t, VNext s) -> eval $ Next $ App t s
    (x,y) -> throwError $ "Invalid arguments to LApp:\n" ++ treeTerm exp

-- Pair creation
eval exp@(Pair e1 e2) = return $ VPair e1 e2

-- First projection
eval exp@(Fst e) = eval e >>= \case
    VPair v1 v2 -> eval v1;
    err -> throwError $ "Took fst of non-pair:\n" ++ treeValue err

-- Second projection
eval exp@(Snd e) = eval e >>= \case
    VPair v1 v2 -> eval v2
    err -> throwError $ "Took snd of non-pair:\n" ++ treeValue err

-- Normal distribtion sampling
eval exp@(Norm e) = eval e >>= \case
    VPair e1 e2 -> match2 eval e1 e2 >>= \case
        (VVal (Fract m), VVal (Fract v)) -> performDraw normalDist [m,v]
        err -> throwError $ "Normal pair does not contain reals: " ++ show err
    _ -> throwError $ "Normal argument not a pair of reals: \n" ++ treeTerm exp

-- Random uniform distrubition sampling
eval Rand = performDraw randDist []

-- Evaluate into values
eval exp@(Force e) = eval e >>= forceEval

-- If then else
eval exp@(Ite b e1 e2) = eval b >>= \case
    VBVal True  -> eval e1
    VBVal False -> eval e2
    _ -> throwError $ "If with non boolean condition:\n" ++ treeTerm exp

-- Coproduct injection
eval exp@(InL e) = return $ VInL e
eval exp@(InR e) = return $ VInR e

-- Matching coproducts
eval exp@(Match e x e1 y e2) = eval e >>= \case
    VInL l  -> eval l >>= eval . substitute e1 x . toExp
    VInR r  -> eval r >>= eval . substitute e2 y . toExp
    _       -> throwError $ "Match on non-coproduct:\n" ++ treeTerm exp

-- Function abstraction
eval exp@(Abstr x e) = return $ VThunk exp

-- Recursion
eval exp@(Rec x e) = eval $ substitute e x $ Next $ recName exp

-- Prev: next inverse
-- Empty substitution list, simply remove the next
eval exp@(Prev (Env []) e) = eval e >>= \case
    VNext e -> eval e
    _ -> throwError $ "Took prev of non-next:\n" ++ treeTerm exp
-- Non empty list, perform substitutions
eval exp@(Prev (Env l) e) = eval $ Prev (Env []) $ substList e l

-- Boxing  and unboxing
eval exp@(Box l e) = return $ VBox l e
eval exp@(Unbox e) = eval e >>= \case
    VBox (Env l) e1 -> eval $ substList e1 l
    _ -> throwError $ "Unbox on non-box:\n" ++ treeTerm exp

-- List operations
eval exp@(LCons e1 e2) = match2 eval e1 e2 >>= \case
    (r, VList l) -> return $ VList $ toExp r : l
    _ -> throwError $ "Invalid arguments to list cons:\n" ++ treeTerm exp

eval exp@(LAppend e1 e2) = match2 eval e1 e2 >>= \case
    (VList l1, VList l2) -> return $ VList ( l1 ++ l2 )
    _ -> throwError $ "Invalid arguments to list append:\n" ++ treeTerm exp

eval exp@(LIndex e1 e2) = match2 eval e1 e2 >>= \case
    (VList l, VVal ( Whole n ) ) -> eval ( l !! fromIntegral n )
    (a1, a2) -> throwError $ "Invalid arguments to list index:\nArgument1:"
        ++ treeValue a1 ++ "\nArgument2:\n" ++ treeValue a2

eval exp@(LHead e) = eval e >>= \case
    VList []    -> throwError "Head on empty list"
    VList l     -> eval $ head l
    _ -> throwError $ "Head on non-list:\n" ++ treeTerm exp

eval exp@(LTail e) = eval e >>= \case
    VList []    -> throwError "Tail on empty list"
    VList l     -> return $ VList $ tail l
    _ -> throwError $ "Tail on non-list:\n" ++ treeTerm exp

eval exp@(LNull e) = eval e >>= \case
    VList l     -> return $ VBVal $ Prelude.null l
    _ -> throwError $ "Null on non-list:\n" ++ treeTerm exp

eval exp@(LLength e) = eval e >>= \case
    VList l     -> return $ VVal $ Whole $ fromIntegral $ length l
    _ -> throwError $ "Length on non-list:\n" ++ treeTerm exp

eval exp@(LFold e1 e2 e3) = match3 eval e1 e2 e3 >>= \case
    (VThunk f, val, VList l) -> do
        let test = Prelude.foldl (App . App f) (toExp val) l
        eval test
    _ -> throwError $ "Invalid arguments to fold:\n" ++ treeTerm exp

eval exp@(LMap e1 e2) = match2 eval e1 e2 >>= \case
    (VThunk f, VList l) -> VList <$> mapM ( fmap toExp . eval . App f ) l
    _ -> throwError $ "Invalid arguments to map:\n" ++ treeTerm exp

eval exp@(LElem e1 e2) = match2 eval e1 e2 >>= \case
    (el, VList l) -> return $ VBVal $ elem (toExp el) l
    _ -> throwError $ "Invalid arguments to elem:\n" ++ treeTerm exp

eval exp@(LTake e1 e2) = match2 eval e1 e2 >>= \case
    (VVal (Whole n), VList l) -> return $ VList $ take (fromIntegral n) l
    _ -> throwError $ "Invalid arguments to take:\n" ++ treeTerm exp

eval exp@(LDrop e1 e2) = match2 eval e1 e2 >>= \case
    (VVal (Whole n), VList l) -> return $ VList $ drop (fromIntegral n) l
    _ -> throwError $ "Invalid arguments to drop:\n" ++ treeTerm exp

eval exp = case exp of
    -- Evaluate into these values
    List l      -> VList <$> mapM (fmap toExp . eval) l

    -- Instant values
    Val v       -> return $ VVal v
    BVal v      -> return $ VBVal $ toBool v

    Min e       -> evalAExp1 eval negate e
    Pow e1 e2   -> evalAExp eval e1 numPow e2
    Div e1 e2   -> evalAExp eval e1 numDiv e2
    Mod e1 e2   -> evalAExp eval e1 numMod e2
    Add e1 e2   -> evalAExp eval e1 (+) e2
    Sub e1 e2   -> evalAExp eval e1 (-) e2
    Mul e1 e2   -> evalAExp eval e1 (*) e2

    Not e       -> evalBExp1 eval not e
    And e1 e2   -> evalBExp eval e1 (&&) e2
    Or e1 e2    -> evalBExp eval e1 (||) e2

    Eq e1 e2    -> evalRelop eval e1 (==) e2
    Lt e1 e2    -> evalRelop eval e1 (<) e2
    Gt e1 e2    -> evalRelop eval e1 (>) e2
    Leq e1 e2   -> evalRelop eval e1 (<=) e2
    Geq e1 e2   -> evalRelop eval e1 (>=) e2
    Single      -> return VSingle

-- Evluates everything underneath certain values to make it readable
forceEval :: Value -> EvalMonad Value
forceEval = \case
    VNext e     -> asks snd >>= \x ->
        if x == 0 then return $ VUNext e
        else local (second $ subtract 1) (VENext <$> (eval e >>= forceEval))
    
    VPair e1 e2 -> VEPair   <$> (eval e1 >>= forceEval) 
                            <*> (eval e2 >>= forceEval)
    VIn e       -> VEIn     <$> (eval e >>= forceEval)
    VInL e      -> VEInL    <$> (eval e >>= forceEval)
    VInR e      -> VEInR    <$> (eval e >>= forceEval)
    VBox l e    -> VEBox    <$> (eval e >>= forceEval)
    other -> return other

