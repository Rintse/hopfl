module Substitution where

import Control.Monad.Reader
import Control.Applicative
import Control.Monad.State
import Data.HashMap.Lazy as HM
import Data.Bifunctor
import Data.Char
import Data.Sequence as Seq
import Debug.Trace

import Syntax.Abs

-- Marks each variable with its depth (w.r.t. binders, i.e. lambda, fix)
-- to avoid having to rename during substition. Variable names are prepended
-- with their depth when bound
-- NOTE: The resulting name is illegal, preventing collisions

uniqNames :: Exp -> Exp
uniqNames e = evalState (reName e) (0, HM.empty)

-- State environment contains a counter and a hashmap in which the values 
-- track all the names that we are currently substituting the key for.
-- The last element in the sequence is the name to substitute the key for.
type MarkMap = (Integer, HashMap String [String])

-- Increments counter and pushes new substitute for x onto m[x]
pushVar :: Exp -> MarkMap -> MarkMap
pushVar (Var (Ident x)) (c, m) = (c+1, HM.insertWith (++) x [x ++ show (c+1)] m)

-- Pops the latest substitute for x off m[x]
popVar :: Exp -> MarkMap -> MarkMap
popVar (Var (Ident x)) (c, m) = (c, HM.adjust tail x m)

-- Gets the latest substitute for x from m[x] (returns x if none are found)
getSub :: Exp -> MarkMap -> Exp
getSub (Var (Ident x)) (c, m) = Var $ Ident (
    case HM.lookup x m of
    Just (s:_) -> s
    _          -> x )

-- TODO check for faulty programs
reName :: Exp -> State MarkMap Exp
reName e = case e of
    Var v           -> gets (getSub e)
    Val v           -> return $ Val v
    Next e          -> Next <$> reName e
    In e            -> In <$> reName e
    Out e           -> Out <$> reName e
    App e1 e2       -> liftA2 App (reName e1) (reName e2)
    LApp e1 o e2    -> liftA3 LApp (reName e1) (return o) (reName e2)
    Pair e1 e2      -> liftA2 Pair (reName e1) (reName e2)
    Fst e           -> Fst <$> reName e
    Snd e           -> Snd <$> reName e
    Norm e          -> Norm <$> reName e
    Abstr l x e     -> do
        modify (pushVar x)
        r1 <- reName x
        r2 <- reName e
        modify (popVar x)
        return $ Abstr l r1 r2
    Rec x e         -> do
        modify (pushVar x)
        r1 <- reName x
        r2 <- reName e
        modify (popVar x)
        return $ Rec r1 r2
    Typed e t       -> Typed <$> reName e <*> return t


-- Prerequesite: The program tree is renamed using uniqNames
-- Substitutes, in exp, x for s
substitute :: Exp -> String -> Exp -> Exp
substitute exp x s = case exp of
    Var (Ident v)   -> if v == x then s else exp
    Val v           -> exp
    Next e          -> Next $ substitute e x s
    In e            -> In $ substitute e x s
    Out e           -> Out $ substitute e x s
    App e1 e2       -> App (substitute e1 x s) (substitute e2 x s)
    LApp e1 o e2    -> LApp (substitute e1 x s) o (substitute e2 x s)
    Pair e1 e2      -> Pair (substitute e1 x s) (substitute e2 x s)
    Fst e           -> Fst $ substitute e x s
    Snd e           -> Snd $ substitute e x s
    Norm e          -> Norm $ substitute e x s
    Abstr l v e     -> Abstr l v (substitute e x s)
    Rec v e         -> Rec v (substitute e x s)
    Typed e t       -> Typed (substitute e x s) t

-- Removes a binder and substitutes the bound occurances
-- Used for performing application and recursion
removeBinder :: Exp -> Exp -> Exp
removeBinder exp s = case exp of
    Abstr l (Var (Ident x)) e   -> substitute e x s
    Rec (Var (Ident x)) e       -> substitute e x exp
    _ -> exp

