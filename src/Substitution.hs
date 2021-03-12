module Substitution where

import Syntax.Abs

import Control.Monad.Reader
import Control.Applicative
import Control.Monad.State
import Data.HashMap.Lazy as HM
import Data.Bifunctor
import Debug.Trace
import Data.Char

-- Renames all variables to unique names to make substitution trivial
uniqNames :: Exp -> Exp
uniqNames e = evalState (reName e) (0, HM.empty)

-- State environment contains a counter and a hashmap in which the values 
-- track all the names that we are currently substituting the key for.
-- The last element in the sequence is the name to substitute the key for.
type NameMap = (Integer, HashMap String [String])

-- Increments counter and pushes new substitute for x onto m[x]
pushVar :: Ident -> NameMap -> NameMap
pushVar (Ident x) (c, m) = (c+1, HM.insertWith (++) x [show (c+1) ++ x] m)

-- Pops the latest substitute for x off m[x]
popVar :: Ident -> NameMap -> NameMap
popVar (Ident x) = second (HM.adjust tail x)
-- popVar (Var (Ident x)) (c, m) = (c, HM.adjust tail x m)

-- Gets the latest substitute for x from m[x] (returns x if none are found)
getSub :: Ident -> NameMap -> Ident
getSub (Ident x) (_, m) = Ident (
    case HM.lookup x m of
    Just (s:_) -> s
    _          -> x )

-- TODO check for faulty programs
reName :: Exp -> State NameMap Exp
reName e = case e of
    Var v           -> gets (Var . getSub v)
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
    Ite b e1 e2     -> liftA3 Ite (reName b) (reName e1) (reName e2)
    Case e x l y r -> do -- TODO
        s   <- reName e
        modify (pushVar x)
        rx <- gets (getSub x)
        rl  <- reName l
        modify (popVar x)
        modify (pushVar y)
        ry <- gets (getSub y)
        rr  <- reName r
        modify (popVar y)
        return $ Case s rx rl ry rr
    Abstr l x e     -> do
        modify (pushVar x)
        r1 <- gets (getSub x)
        r2 <- reName e
        modify (popVar x)
        return $ Abstr l r1 r2
    Rec x e         -> do
        modify (pushVar x)
        r1 <- gets (getSub x)
        r2 <- reName e
        modify (popVar x)
        return $ Rec r1 r2
    Add e1 e2       -> liftA2 Add (reName e1) (reName e2)
    Sub e1 e2       -> liftA2 Sub (reName e1) (reName e2)
    Mul e1 e2       -> liftA2 Mul (reName e1) (reName e2)
    Div e1 e2       -> liftA2 Div (reName e1) (reName e2)
    And e1 o e2     -> liftA3 And (reName e1) (return o) (reName e2)
    Or e1 o e2      -> liftA3 Or (reName e1) (return o) (reName e2)
    Eq e1 e2        -> liftA2 Eq (reName e1) (reName e2)
    Lt e1 e2        -> liftA2 Lt (reName e1) (reName e2)
    Gt e1 e2        -> liftA2 Gt (reName e1) (reName e2)
    Leq e1 o e2     -> liftA3 Leq (reName e1) (return o) (reName e2)
    Geq e1 o e2     -> liftA3 Geq (reName e1) (return o) (reName e2)

isFree :: String -> Bool
isFree s = isLetter $ head s

-- Renames all variables in body of rec statement to avoid
-- application substitution in folded out rec terms
recName :: Exp -> Exp
recName exp = case exp of
    Var (Ident v)       -> if isFree v then exp else Var (Ident $ "$" ++ v)
    Val v               -> exp
    Next e              -> Next $ recName e
    In e                -> In $ recName e
    Out e               -> Out $ recName e
    App e1 e2           -> App (recName e1) (recName e2)
    LApp e1 o e2        -> LApp (recName e1) o (recName e2)
    Pair e1 e2          -> Pair (recName e1) (recName e2)
    Fst e               -> Fst $ recName e
    Snd e               -> Snd $ recName e
    Norm e              -> Norm $ recName e
    Ite b e1 e2         -> Ite (recName b) (recName e1) (recName e2)
    Case e (Ident x) l (Ident y) r -> Case -- TODO
        (recName e)
        (Ident $ "$" ++ x) (recName l)
        (Ident $ "$" ++ y) (recName r)
    Abstr l (Ident v) e -> Abstr l (Ident $ "$" ++ v) (recName e)
    Rec (Ident v) e     -> Rec (Ident $ "$" ++ v) (recName e)
    Add e1 e2           -> Add (recName e1) (recName e2)
    Sub e1 e2           -> Sub (recName e1) (recName e2)
    Mul e1 e2           -> Mul (recName e1) (recName e2)
    Div e1 e2           -> Div (recName e1) (recName e2)
    And e1 o e2         -> And (recName e1) o (recName e2)
    Or e1 o e2          -> Or (recName e1) o (recName e2)
    Eq e1 e2            -> Eq (recName e1) (recName e2)
    Lt e1 e2            -> Lt (recName e1) (recName e2)
    Gt e1 e2            -> Gt (recName e1) (recName e2)
    Leq e1 o e2         -> Leq (recName e1) o (recName e2)
    Geq e1 o e2         -> Geq (recName e1) o (recName e2)

-- Prerequesite: The program tree is renamed using uniqNames
-- Substitutes, in exp, x for s
subst :: Exp -> Reader (String, Exp) Exp
subst exp = case exp of
    Var (Ident v)   -> asks (\(x,s) -> if v == x then s else exp)
    Val v           -> return exp
    Next e          -> Next <$> subst e
    In e            -> In <$> subst e
    Out e           -> Out <$> subst e
    App e1 e2       -> liftA2 App (subst e1) (subst e2)
    LApp e1 o e2    -> liftA3 LApp (subst e1) (return o) (subst e2)
    Pair e1 e2      -> liftA2 Pair (subst e1) (subst e2)
    Fst e           -> Fst <$> subst e
    Snd e           -> Snd <$> subst e
    Norm e          -> Norm <$> subst e
    Ite b e1 e2     -> liftA3 Ite (subst b) (subst e1) (subst e2)
    Case e x l y r  -> Case <$> subst e <*> -- TODO 
                       return x <*> subst l <*> 
                       return y <*> subst r
    Abstr l v e     -> Abstr l v <$> subst e
    Rec v e         -> Rec v <$> subst e
    Add e1 e2       -> liftA2 Add (subst e1) (subst e2)
    Sub e1 e2       -> liftA2 Sub (subst e1) (subst e2)
    Mul e1 e2       -> liftA2 Mul (subst e1) (subst e2)
    Div e1 e2       -> liftA2 Div (subst e1) (subst e2)
    And e1 o e2     -> liftA3 And (subst e1) (return o) (subst e2)
    Or e1 o e2      -> liftA3 Or (subst e1) (return o) (subst e2)
    Eq e1 e2        -> liftA2 Eq (subst e1) (subst e2)
    Lt e1 e2        -> liftA2 Lt (subst e1) (subst e2)
    Gt e1 e2        -> liftA2 Gt (subst e1) (subst e2)
    Leq e1 o e2     -> liftA3 Leq (subst e1) (return o) (subst e2)
    Geq e1 o e2     -> liftA3 Geq (subst e1) (return o) (subst e2)

-- Removes a binder and substitutes the bound occurances
-- Used for performing application and recursion
removeBinder :: Exp -> Exp -> Exp
removeBinder exp s = case exp of
    Abstr l (Ident x) e   -> runReader (subst e) (x, s)
    Rec (Ident x) e       -> runReader (subst e) (x, recName s)
    _ -> exp

