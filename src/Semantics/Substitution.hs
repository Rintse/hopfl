-- Defines some substitution functions needed for evaluating 
-- recursion, function application, unboxing, previous and match statements

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, LambdaCase #-}

module Semantics.Substitution where

import Syntax.IdAbs
import Syntax.Number

import Data.Functor.Foldable
import Data.Functor.Foldable.Monadic
import Control.Monad.Reader
import Control.Monad.State

-- Increases the recursion depth of an idenitifier by 1
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
    Var (Ident x 0 d)   -> VarF $ Ident x 0 d -- Free variable
    Var v               -> VarF $ incDepth v -- Not free, increment
    Prev l e            -> PrevF (recNameL l) e
    Box l e             -> BoxF (recNameL l) e
    Abstr x e           -> AbstrF (incDepth x) e
    Rec x e             -> RecF  (incDepth x) e
    Match e x l y r     -> MatchF e (incDepth x) l (incDepth y) r
    other               -> project other

-- Performs substitution
-- Variables are the same if they have the same name, id and depth
doSub :: Ident -> (Ident, Exp) -> Exp
doSub v@(Ident x idx dx) (Ident y idy dy, s) =
    if x == y && idx == idy && dx == dy then s else Var v

-- Substitutes in substitution lists.
-- Used in Prev and Box
substL :: Environment -> Reader (Ident, Exp) Environment
substL (Env l) = Env <$> mapM (\(Assign x t) -> Assign x <$> subst t) l

-- Substitutes, in exp, x for s
subst :: Exp -> Reader (Ident, Exp) Exp
subst = apoM $ \case
    Var x    -> asks (fmap Left . project . doSub x)
    Prev l e -> PrevF <$> substL l <*> return (Right e)
    Box l e  -> BoxF <$> substL l <*> return (Right e)
    other    -> return $ Right <$> project other

-- Substitutes in exp, s for x
substitute :: Exp -> Ident -> Exp -> Exp
substitute exp x s = runReader (subst exp) (x,s)

-- Perform substitution for a list of subs
substList :: Exp -> [Assignment] -> Exp
substList = Prelude.foldl (\e (Assign x t) -> substitute e x t)

