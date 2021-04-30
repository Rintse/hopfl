
{-# LANGUAGE LambdaCase #-}
module Tools.Preprocess where

import Syntax.Raw.Abs
import Syntax.AbsF
import Semantics.Substitution

import Data.Functor.Foldable.TH
import Data.Functor.Foldable
import Data.Functor.Foldable.Monadic
import Control.Monad.Reader
import Debug.Trace

-- Perform a single definition substitution
defSub :: Exp -> Reader Assignment Exp
defSub = anaM $ \case
    e@(Var (Ident x)) -> do
        (Assign (Ident s) o t) <- ask
        if x == s
            then return $ project t
            else return $ project e
    Prev (Env l) e -> do
        a <- ask
        let rl = Env $ map (\(Assign x o t) -> Assign x o $ runDef t a) l
        return $ PrevF rl e
    Box (Env l) e -> do
        a <- ask
        let rl = Env $ map (\(Assign x o t) -> Assign x o $ runDef t a) l
        return $ PrevF rl e
    other -> return $ project other

-- Runs definition substitution inside a reader monad
runDef :: Exp -> Assignment -> Exp
runDef e = runReader (defSub e)

-- Perform a definition substitution all following definitions
inDef :: [Assignment] -> Assignment -> [Assignment]
inDef l a = map (\(Assign x o t) -> Assign x o (runDef t a)) l

-- Perform all definition subs in the definitions after it
inDefs :: [Assignment] -> [Assignment]
inDefs [a] = [a]
inDefs (a:l) = a : inDefs (inDef l a)

-- Perform definition substitutions for a list of defs
handleDefs :: Prg -> Exp
handleDefs (DefProg (Env l) e) = foldl runDef e $ inDefs l
handleDefs (Prog e) = e

