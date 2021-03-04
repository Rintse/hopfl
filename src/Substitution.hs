module Substitution where

import Control.Monad.Reader
import Control.Applicative
import Control.Monad.State
import Data.HashMap.Lazy as HM
import Data.Char

import Syntax.Abs

-- Marks each variable with its depth (w.r.t. binders, i.e. lambda, fix)
-- to avoid having to rename during substition. Variable names are prepended
-- with their depth when bound
-- NOTE: The resulting name is illegal, preventing collisions
markVars :: Exp -> Exp
markVars e = runReader (markSub e) HM.empty

-- Reader environment contains a hashmap that tracks
-- how many abstractions using the same variable deep we are

-- Adds a new variable to the environment (or updates existing)
addVar :: Exp -> HashMap String Integer -> HashMap String Integer
addVar (Var (Ident x)) = insertWith (+) x 1

-- Returns a string representing the depth of a given variable
getDepth :: String -> HashMap String Integer -> String
getDepth x l = maybe "" show (HM.lookup x l)

-- Reader to be run by markVars
markSub :: Exp -> Reader (HashMap String Integer) Exp
markSub e = case e of
    Var (Ident x)   -> asks (\l -> Var $ Ident (getDepth x l ++ x))
    Val v           -> return $ Val v
    Next e          -> Next <$> markSub e
    In e            -> In <$> markSub e
    Out e           -> Out <$> markSub e
    App e1 e2       -> App <$> markSub e1 <*> markSub e2
    LApp e1 o e2    -> LApp <$> markSub e1 <*> return o <*> markSub e2
    Pair e1 e2      -> Pair <$> markSub e1 <*> markSub e2
    Fst e           -> Fst <$> markSub e
    Snd e           -> Snd <$> markSub e
    Norm e          -> Norm <$> markSub e
    Abstr l x e     -> Abstr l <$> local (addVar x) (markSub x) <*> local (addVar x) (markSub e)
    Rec x e         -> Rec <$> local (addVar x) (markSub x) <*> local (addVar x) (markSub e)
    Typed e t       -> Typed <$> markSub e <*> return t


-- Prerequesite: The program tree is marked using markVars
-- Checks whether a variable is free
isFree :: Exp -> Bool
isFree (Var (Ident e)) = not (isDigit $ head e)

-- Strips the marked variable name by removing the leading $ or digits
stripName :: String -> String
stripName = dropWhile isDigit

-- Prerequesite: The program tree is marked using markVars
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


