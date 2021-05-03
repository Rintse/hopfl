{-# LANGUAGE LambdaCase #-}
module Tools.Preprocess where

import Syntax.Raw.Abs
import Syntax.AbsF
import Semantics.Substitution
import Semantics.Builtins

import qualified Data.Set as Set
import Data.Functor.Foldable.TH
import Data.Functor.Foldable
import Data.Functor.Foldable.Monadic
import Control.Monad.Reader
import System.Exit
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
        return $ BoxF rl e
    EList (List (Elems l)) -> do
        a <- ask
        let rl = Elems $ map (\(Elem e) -> Elem $ runDef e a) l
        return $ EListF $ List rl
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

toSet :: [Assignment] -> Set.Set String
toSet l = Set.fromList $ map (\(Assign (Ident x) _ _) -> x) l

-- Perform definition substitutions for a list of defs
handleDefs :: Prg -> IO Exp
handleDefs (DefProg (Env l) e) = do
    let usedBuiltinNames = Set.intersection (toSet builtins) (toSet l)

    unless ( null usedBuiltinNames ) ( do
        putStrLn "Error. Used reserved name of builtin:"
        mapM_ putStrLn (Set.toList usedBuiltinNames)
        exitFailure )

    return $ foldl runDef e $ inDefs (l ++ builtins)
handleDefs (Prog e) = return e

-- A list is repeated pairing in InR followed by the singleton in InL
desugarList :: [El] -> Exp
desugarList l = do
    let end = InL $ Single $ TSingle ""
    foldr ((\x y -> InR $ Pair x y) . (\(Elem e) -> e)) end l

-- Desugar lists
desugarLists :: Exp -> Exp
desugarLists = ana $ \case
    EList (List (Elems l)) -> project $ desugarList l
    other -> project other

