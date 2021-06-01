-- Defines a custom datastructure to contain the program, which is 
-- nearly identical to the raw parsed data, except that identifiers
-- are annotated with a unique id to aid in substitution. Also defines a 
-- function that transforms raw expressions into their annotated versions

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, LambdaCase #-}

module Preprocess.AnnotateVars where

import qualified Syntax.Raw.Abs as Raw

import Syntax.Number
import Syntax.Expression

import qualified Data.Set as Set
import Data.Bifunctor
import Data.Functor.Foldable.TH
import Data.Functor.Foldable
import Control.Monad.Reader
import Control.Applicative
import Control.Monad.State
import qualified Data.HashMap.Lazy as HM
import Data.List.Index
import Debug.Trace


-- State environment contains a counter and a hashmap in which the values 
-- track all the names that we are currently substituting the key for.
-- The last element in the sequence is the name to substitute the key for.
type IdMap = HM.HashMap String [Int]
newtype IdMonad a = IdMonad {
    runId :: StateT Int (Reader IdMap) a
} deriving (    Functor, Applicative, Monad,
                MonadState Int,
                MonadReader IdMap   )

-- Increments counter and pushes new substitute for x onto m[x]
pushVar :: Raw.Ident -> Int -> IdMap -> IdMap
pushVar (Raw.Ident x) c = HM.insertWith (++) x [c]

-- Pushes all variables in a substitution list
pushVars :: [Raw.Assignment] -> Int -> IdMap -> IdMap
pushVars l c m = do
    let idxd = Prelude.map (first (+c)) (indexed l)
    let push1 = \m1 (id, Raw.Assign x _ t) -> pushVar x id m1
    Prelude.foldl push1 m idxd

-- Rename an individual substitution
varAssign :: Raw.Assignment -> IdMonad Assignment
varAssign (Raw.Assign x _ t) = do
    cur <- modify (+1) >> get -- x is not bound in t
    asks (Assign . (getSub x . pushVar x cur)) <*> transform t

-- Gets the latest substitute for x from m[x] (returns x if none are found)
getSub :: Raw.Ident -> IdMap -> Ident
getSub (Raw.Ident x) m = Ident x (head $ HM.findWithDefault [0] x m) 0

-- Gets all free variables in an assignment list
getFreesL :: [Assignment] -> Set.Set Ident
getFreesL = foldr (Set.union . (\(Assign x t) -> getFrees t)) Set.empty

-- Gets all free variables in an expression
getFrees :: Exp -> Set.Set Ident
getFrees = cata $ \case
    (VarF id@(Ident x d _)) -> if d == 0 then Set.singleton id else Set.empty
    (ValF _)                -> Set.empty
    (BValF _)               -> Set.empty
    (PrevF (Env l) e)       -> Set.union e $ getFreesL l
    (BoxF (Env l) e)        -> Set.union e $ getFreesL l
    fFree                   -> foldr Set.union Set.empty fFree

-- Transforms an identifier into an identity substitution for that identifier
idSubst :: Ident -> Raw.Assignment
idSubst (Ident x _ _) = Raw.Assign
    (Raw.Ident x) (Raw.TSub "") (Raw.Var $ Raw.Ident x)

-- Returns the identity substitution list for all free variables
-- in term e. Used in boxF and prevF
freeList :: Raw.Exp -> Raw.Environment
freeList e = Raw.Env $ Prelude.map idSubst $ Set.toList $ getFrees (annotateVars e)

-- TODO check for faulty programs?
-- Transforms the raw syntax tree into a version where the 
-- idenfiers are made unique with an id and recursion depth tag.
transform :: Raw.Exp -> IdMonad Exp
transform exp = case exp of
    -- Annotate variables with a unique ID
    Raw.Var v           -> asks (Var . getSub v)
    
    -- Integers and doubles into one overarching number type
    Raw.DVal v          -> return $ Val $ Fract v
    Raw.IVal v          -> return $ Val $ Whole v
    
    -- Simple 1-to-1 correspondence.
    Raw.Single t        -> return Single
    Raw.BVal v          -> return $ BVal v
    Raw.Unbox e         -> fmap   Unbox (transform e)
    Raw.Force e         -> fmap   Force (transform e)
    Raw.FColist e       -> fmap   FColist (transform e)
    Raw.Norm e          -> fmap   Norm  (transform e)
    Raw.Next e          -> fmap   Next  (transform e)
    Raw.Out e           -> fmap   Out   (transform e)
    Raw.Fst e           -> fmap   Fst   (transform e)
    Raw.Snd e           -> fmap   Snd   (transform e)
    Raw.InL e           -> fmap   InL   (transform e)
    Raw.InR e           -> fmap   InR   (transform e)
    Raw.Not _ e         -> fmap   Not   (transform e)
    Raw.Min e           -> fmap   Min   (transform e)
    Raw.In e            -> fmap   In    (transform e)
    Raw.LApp e1 _ e2    -> liftA2 LApp  (transform e1) (transform e2)
    Raw.Pair e1 e2      -> liftA2 Pair  (transform e1) (transform e2)
    Raw.Leq e1 o e2     -> liftA2 Leq   (transform e1) (transform e2)
    Raw.Geq e1 o e2     -> liftA2 Geq   (transform e1) (transform e2)
    Raw.App e1 e2       -> liftA2 App   (transform e1) (transform e2)
    Raw.Add e1 e2       -> liftA2 Add   (transform e1) (transform e2)
    Raw.Sub e1 e2       -> liftA2 Sub   (transform e1) (transform e2)
    Raw.Mul e1 e2       -> liftA2 Mul   (transform e1) (transform e2)
    Raw.Mod e1 e2       -> liftA2 Mod   (transform e1) (transform e2)
    Raw.Pow e1 e2       -> liftA2 Pow   (transform e1) (transform e2)
    Raw.Div e1 e2       -> liftA2 Div   (transform e1) (transform e2)
    Raw.And e1 o e2     -> liftA2 And   (transform e1) (transform e2)
    Raw.Or e1 o e2      -> liftA2 Or    (transform e1) (transform e2)
    Raw.Eq e1 e2        -> liftA2 Eq    (transform e1) (transform e2)
    Raw.Lt e1 e2        -> liftA2 Lt    (transform e1) (transform e2)
    Raw.Gt e1 e2        -> liftA2 Gt    (transform e1) (transform e2)
    Raw.Ite b e1 e2     -> liftA3 Ite   (transform b)
                                        (transform e1) (transform e2)

    -- Syntactic sugar
    Raw.PrevE e         -> transform $ Raw.Prev (Raw.Env []) e
    Raw.BoxI e          -> transform $ Raw.Box (freeList e) e
    Raw.PrevI e         -> transform $ Raw.Prev (freeList e) e

    -- WARNING: Here be binders
    Raw.Box (Raw.Env l) e -> do
        cur <- gets (+1)
        rl <- mapM varAssign l
        re <- local (pushVars l cur) $ transform e -- vars in l are bound in e
        return $ Box (Env rl) re
    Raw.Prev (Raw.Env l) e -> do
        cur <- gets (+1)
        rl <- mapM varAssign l
        re <- local (pushVars l cur) $ transform e -- all vars in l are bound in e
        return $ Prev (Env rl) re
    Raw.Match e x _ l y _ r -> do
        re <- transform e -- Nothing binds e
        cur <- modify (+1) >> get -- x is bound in l
        rx <- asks (getSub x . pushVar x cur)
        rl <- local (pushVar x cur) $ transform l
        cur <- modify (+1) >> get -- y is bound in r
        ry <- asks (getSub y . pushVar y cur)
        rr <- local (pushVar y cur) $ transform r
        return $ Match re rx rl ry rr
    Raw.Abstr _ x e -> do
        cur <- modify (+1) >> get -- x is bound in e
        r1 <- asks (getSub x . pushVar x cur)
        r2 <- local (pushVar x cur) $ transform e
        return $ Abstr r1 r2
    Raw.Rec f e -> do
        cur <- modify (+1) >> get -- f is bound in e
        r1 <- asks (getSub f . pushVar f cur)
        r2 <- local (pushVar f cur) $ transform e
        return $ Rec r1 r2

-- Translate a raw tree into the id tree with annotated identifiers
annotateVars :: Raw.Exp -> Exp
annotateVars e = runReader (evalStateT (runId(transform e)) 0) HM.empty

