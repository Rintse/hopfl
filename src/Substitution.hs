module Substitution where

import Control.Monad.Reader
import Control.Applicative
import Control.Monad.State
import Data.HashMap.Lazy as HM
import Data.Char

import Syntax.Abs

-- markVars :: Exp -> Exp
-- markVars e = evalState (markSub e) 0 

-- markSub :: Exp -> State Integer Exp
-- markSub e = case e of
    -- Var (Ident x)   -> modify (+1) >> gets (\l -> Var $ Ident (x ++ show l))
    -- Val v           -> return $ Val v
    -- Next e          -> Next <$> markSub e
    -- In e            -> In <$> markSub e
    -- Out e           -> Out <$> markSub e
    -- App e1 e2       -> App <$> markSub e1 <*> markSub e2
    -- LApp e1 o e2    -> LApp <$> markSub e1 <*> return o <*> markSub e2
    -- Pair e1 e2      -> Pair <$> markSub e1 <*> markSub e2
    -- Fst e           -> Fst <$> markSub e
    -- Snd e           -> Snd <$> markSub e
    -- Norm e          -> Norm <$> markSub e
    -- Abstr l x e     -> Abstr l <$> markSub x <*> markSub e
    -- Rec x e         -> Rec <$> markSub x <*> markSub e
    -- Typed e t       -> Typed <$> markSub e <*> return t

-- Marks each variable with its depth (w.r.t. binders, i.e. lambda, fix)
-- To avoid having to rename during substition. Variable names are prepended
-- with their depth when bound, or "$" when free
-- NOTE: Both prepends results in illegal var names, preventing collisions
markVars :: Exp -> Exp
markVars e = runReader (markSub e) HM.empty

addVar :: Exp -> HashMap String Integer -> HashMap String Integer
addVar (Var (Ident x)) = insertWith (+) x 1

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
-- Substitutes x (if free) for s in e
substitute :: Exp -> Exp -> Exp -> Exp
substitute exp x s = case exp of
    Var x           -> if isFree exp then s else exp
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

removeBinder :: Exp -> Exp -> Exp
removeBinder e s = case e of
    Abstr l x e -> e
    Rec x e     -> e
    e           -> e


