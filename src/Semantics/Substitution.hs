-- Defines some substitution functions needed for evaluating 
-- recursion, function application and match statements

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, LambdaCase #-}

module Semantics.Substitution where

import Syntax.IdAbs

import Data.Functor.Foldable.TH
import Data.Functor.Foldable
import Data.Functor.Foldable.Monadic
import Control.Monad.Reader
import Control.Applicative
import Control.Monad.State
import Data.HashMap.Lazy as HM
import Data.Bifunctor
import Debug.Trace
import Data.Char

incDepth :: Ident -> Ident
incDepth (Ident x id d) = Ident x id (d+1)

-- Renames for uniqueness of substituted values in recursion
-- Used in Prev and Box
recNameL :: Environment -> Environment
recNameL (Env l) = Env $ Prelude.map rec1 l
    where rec1 = \(Assign x t) -> Assign (incDepth x) (recName t)

-- Renames all variables in body of rec statement to avoid
-- faulty application substitution in folded out rec terms
recName :: Exp -> Exp
recName = ana $ \case
    Var (Ident x 0 d)   -> VarF $   Ident x 0 d -- Free variable
    Var v               -> VarF $   incDepth v -- Not free, increment
    Prev l e            -> PrevF    (recNameL l) e
    Box l e             -> BoxF     (recNameL l) e
    Abstr x e           -> AbstrF   (incDepth x) e
    Rec x e             -> RecF     (incDepth x) e
    Match e x l y r     -> MatchF e (incDepth x) l (incDepth y) r
    other               -> project other

-- Performs substitution
-- Variables are the same if they have the same name, id and depth
doSub :: Ident -> (Ident, Exp) -> Exp
doSub v@(Ident x idx dx) (Ident y idy dy, s) =
    if x == y && idx == idy && dx == dy then s
    else Var v

-- Substitutes in substitution lists.
-- Used in Prev and Box
substL :: Environment -> Reader (Ident, Exp) Environment
substL (Env l) = Env <$> mapM (\(Assign x t) -> Assign x <$> subst t) l

subst2 :: Exp -> Reader (Ident, Exp) Exp
subst2 = anaM $ \case
    Var x           -> asks (project . doSub x)

-- Substitutes, in exp, x for s
subst :: Exp -> Reader (Ident, Exp) Exp
subst exp = case exp of
    Var x           -> asks (doSub x)
    Val v           -> return exp
    BVal v          -> return exp
    Next e          -> Next <$> subst e
    Prev l e        -> Prev <$> substL l <*> subst e
    PrevI e         -> PrevI <$> subst e
    PrevE e         -> PrevE <$> subst e
    Box l e         -> Box <$> substL l <*> subst e
    BoxI e          -> BoxI <$> subst e
    In e            -> In <$> subst e
    Out e           -> Out <$> subst e
    App e1 e2       -> liftA2 App (subst e1) (subst e2)
    LApp e1 e2      -> liftA2 LApp (subst e1) (subst e2)
    Pair e1 e2      -> liftA2 Pair (subst e1) (subst e2)
    Fst e           -> Fst <$> subst e
    Snd e           -> Snd <$> subst e
    InL e           -> InL <$> subst e
    InR e           -> InR <$> subst e
    Norm e          -> Norm <$> subst e
    Ite b e1 e2     -> liftA3 Ite (subst b) (subst e1) (subst e2)
    Match e x l y r  -> Match <$> subst e <*>
                        return x <*> subst l <*> return y <*> subst r
    Abstr v e       -> Abstr v <$> subst e
    Rec v e         -> Rec v <$> subst e
    Not e           -> fmap Not     (subst e)
    Add e1 e2       -> liftA2 Add   (subst e1) (subst e2)
    Sub e1 e2       -> liftA2 Sub   (subst e1) (subst e2)
    Mul e1 e2       -> liftA2 Mul   (subst e1) (subst e2)
    Div e1 e2       -> liftA2 Div   (subst e1) (subst e2)
    And e1 e2       -> liftA2 And   (subst e1) (subst e2)
    Or e1 e2        -> liftA2 Or    (subst e1) (subst e2)
    Eq e1 e2        -> liftA2 Eq    (subst e1) (subst e2)
    Lt e1 e2        -> liftA2 Lt    (subst e1) (subst e2)
    Gt e1 e2        -> liftA2 Gt    (subst e1) (subst e2)
    Leq e1 e2       -> liftA2 Leq   (subst e1) (subst e2)
    Geq e1 e2       -> liftA2 Geq   (subst e1) (subst e2)

-- Substitutes in exp, s for x
substitute :: Exp -> Ident -> Exp -> Exp
substitute exp x s = runReader (subst exp) (x,s)

-- Perform substitution for a list of subs
substList :: Exp -> [Assignment] -> Exp
substList = Prelude.foldl (\e (Assign x t) -> substitute e x t)
