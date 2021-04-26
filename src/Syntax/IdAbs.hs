-- Defines a custom datastructure to contain the program, which is 
-- nearly identical to the raw parsed data, except that identifiers
-- are annotated with a unique id to aid in substitution. Also defines a 
-- function that transforms raw expressions into their annotated versions

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, LambdaCase #-}

module Syntax.IdAbs where

import qualified Syntax.Abs as Raw

import qualified Data.Set as Set
import Data.Functor.Foldable.TH
import Data.Functor.Foldable
import Control.Monad.Reader
import Control.Applicative
import Control.Monad.State
import qualified Data.HashMap.Lazy as HM
import Data.List.Index
import Debug.Trace

data Ident = Ident String Int Int
  deriving (Eq, Ord, Show, Read)

data Exp
    = Var Ident
    | Val Double
    | BVal Raw.BConst
    | Next Exp
    | BoxI Exp
    | Unbox Exp
    | Box Environment Exp
    | In    Exp
    | Out   Exp
    | Fst   Exp
    | Snd   Exp
    | InL   Exp
    | InR   Exp
    | PrevE Exp
    | PrevI Exp
    | App Exp Exp
    | LApp Exp Exp
    | Mul Exp Exp
    | Div Exp Exp
    | Add Exp Exp
    | Sub Exp Exp
    | Eq Exp Exp
    | Lt Exp Exp
    | Gt Exp Exp
    | Leq Exp Exp
    | Geq Exp Exp
    | Not Exp
    | And Exp Exp
    | Or Exp Exp
    | Pair Exp Exp
    | Norm Exp
    | Prev Environment Exp
    | Ite Exp Exp Exp
    | Match Exp Ident Exp Ident Exp
    | Abstr Ident Exp
    | Rec Ident Exp
    deriving (Eq, Ord, Show, Read)

data Assignment = Assign Ident Exp
  deriving (Eq, Ord, Show, Read)

newtype Environment = Env [Assignment]
  deriving (Eq, Ord, Show, Read)

-- Make basefunctor to be able to use recursion schemes
makeBaseFunctor ''Exp

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
    let idxd = Prelude.map (\(i,a) -> (i+c,a)) (indexed l)
    let push1 = \m1 (id, Raw.Assign x _ t) -> pushVar x id m1
    Prelude.foldl push1 m idxd

-- Rename an individual substitution
varAssign :: Raw.Assignment -> IdMonad Assignment
varAssign (Raw.Assign x _ t) = do
    cur <- modify (+1) >> get
    sub1 <- asks (getSub x . pushVar x cur)
    -- x is not bound in t
    Assign sub1 <$> reName t

-- Gets the latest substitute for x from m[x] (returns x if none are found)
getSub :: Raw.Ident -> IdMap -> Ident
getSub (Raw.Ident x) m = Ident x (head $ HM.findWithDefault [0] x m) 0

-- Gets all free variables in an expression
getFrees :: Exp -> Set.Set Ident
getFrees = cata $ \case
    (VarF i@(Ident x 0 _))   -> Set.singleton i -- Index 0: Free variable
    (VarF _)                 -> Set.empty
    (BValF _)                -> Set.empty
    (ValF _)                 -> Set.empty
    (PrevF (Env l) e)        -> do 
        let frees = Prelude.map (\(Assign x t) -> getFrees t) l
        let free = Prelude.foldr Set.union Set.empty frees
        Set.union free e
    (BoxF (Env l) e)         -> do
        let frees = Prelude.map (\(Assign x t) -> getFrees t) l
        let free = Prelude.foldr Set.union Set.empty frees
        Set.union free e
    fFree                    -> foldr Set.union Set.empty fFree

-- Transforms an identifier into an identity substitution
-- for that identifier
idToAssign :: Ident -> Raw.Assignment
idToAssign (Ident x _ _) = Raw.Assign
    (Raw.Ident x) (Raw.TSub "") (Raw.Var $ Raw.Ident x)

-- Returns the identity substitution list for all free variables
-- in term e. Used in boxF and prevF
freeList :: Raw.Exp -> IdMonad Raw.Environment
freeList e = do
    renamed <- reName e
    let frees = Set.toList $ getFrees renamed
    let list = Prelude.map idToAssign frees
    return $ Raw.Env list

-- TODO check for faulty programs
reName :: Raw.Exp -> IdMonad Exp
reName exp = case exp of
    Raw.Var v           -> asks (Var . getSub v)
    Raw.Val v           -> return $ Val v
    Raw.BVal v          -> return $ BVal v
    Raw.Next e          -> Next <$> reName e
    Raw.PrevE e         -> reName $ Raw.Prev (Raw.Env []) e
    Raw.PrevF e         -> do
        frees <- freeList e
        reName $ Raw.Prev frees e -- TODO
    Raw.BoxF e          -> reName $ Raw.Box (Raw.Env []) e
    Raw.In e            -> In <$> reName e
    Raw.Out e           -> Out <$> reName e
    Raw.App e1 e2       -> liftA2 App (reName e1) (reName e2)
    Raw.LApp e1 _ e2    -> liftA2 LApp (reName e1) (reName e2)
    Raw.Pair e1 e2      -> liftA2 Pair (reName e1) (reName e2)
    Raw.Fst e           -> Fst <$> reName e
    Raw.Snd e           -> Snd <$> reName e
    Raw.InL e           -> InL <$> reName e
    Raw.InR e           -> InR <$> reName e
    Raw.Norm e          -> Norm <$> reName e
    Raw.Ite b e1 e2     -> liftA3 Ite (reName b) (reName e1) (reName e2)
    Raw.Box (Raw.Env l) e -> do
        cur <- gets (+1)
        rl <- mapM varAssign l
        -- all vars in l are bound in e
        re <- local (pushVars l cur) $ reName e
        return $ Prev (Env rl) re
    Raw.Prev (Raw.Env l) e -> do
        cur <- gets (+1)
        rl <- mapM varAssign l
        -- all vars in l are bound in e
        re <- local (pushVars l cur) $ reName e
        return $ Prev (Env rl) re
    Raw.Match e x _ l y _ r -> do
        -- Nothing binds e
        re <- reName e
        -- x is bound in l
        cur <- modify (+1) >> get
        rx <- asks (getSub x . pushVar x cur)
        rl <- local (pushVar x cur) $ reName l
        -- y is bound in r
        cur <- modify (+1) >> get
        ry <- asks (getSub y . pushVar y cur)
        rr <- local (pushVar y cur) $ reName r
        return $ Match re rx rl ry rr
    Raw.Abstr _ x e -> do
        -- x is bound in e
        cur <- modify (+1) >> get
        r1 <- asks (getSub x . pushVar x cur)
        r2 <- local (pushVar x cur) $ reName e
        return $ Abstr r1 r2
    Raw.Rec f e -> do
        -- f is bound in e
        cur <- modify (+1) >> get
        r1 <- asks (getSub f . pushVar f cur)
        r2 <- local (pushVar f cur) $ reName e
        return $ Rec r1 r2
    Raw.Not _ e         -> fmap Not     (reName e)
    Raw.Add e1 e2       -> liftA2 Add   (reName e1) (reName e2)
    Raw.Sub e1 e2       -> liftA2 Sub   (reName e1) (reName e2)
    Raw.Mul e1 e2       -> liftA2 Mul   (reName e1) (reName e2)
    Raw.Div e1 e2       -> liftA2 Div   (reName e1) (reName e2)
    Raw.And e1 o e2     -> liftA2 And   (reName e1) (reName e2)
    Raw.Or e1 o e2      -> liftA2 Or    (reName e1) (reName e2)
    Raw.Eq e1 e2        -> liftA2 Eq    (reName e1) (reName e2)
    Raw.Lt e1 e2        -> liftA2 Lt    (reName e1) (reName e2)
    Raw.Gt e1 e2        -> liftA2 Gt    (reName e1) (reName e2)
    Raw.Leq e1 o e2     -> liftA2 Leq   (reName e1) (reName e2)
    Raw.Geq e1 o e2     -> liftA2 Geq   (reName e1) (reName e2)

-- Translate a raw tree into the id tree with annotated identifiers
idExp :: Raw.Exp -> Exp
idExp e = runReader (evalStateT (runId (reName e)) 0 ) HM.empty
