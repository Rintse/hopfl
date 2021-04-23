-- Defines a custom datastructure to contain the program, which is 
-- nearly identical to the raw parsed data, except that identifiers
-- are annotated with a unique id to aid in substitution. Also defines a 
-- function that transforms raw expressions into their annotated versions

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Syntax.IdAbs where

import qualified Syntax.Abs as Raw

import Control.Monad.Reader
import Control.Applicative
import Control.Monad.State
import Data.HashMap.Lazy as HM
import Data.List.Index
import Debug.Trace

data Ident = Ident String Int Int
  deriving (Eq, Ord, Show, Read)

data Exp
    = Var Ident
    | Val Double
    | BVal Raw.BConst
    | Next Exp
    | Box Environment Exp
    | Unbox Exp
    | Prev Environment Exp
    | PrevE Exp
    | PrevF Exp
    | In Exp
    | Out Exp
    | Fst Exp
    | Snd Exp
    | InL Exp
    | InR Exp
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
    | Ite Exp Exp Exp
    | Match Exp Ident Exp Ident Exp
    | Abstr Ident Exp
    | Rec Ident Exp
    deriving (Eq, Ord, Show, Read)

data Assignment = Assign Ident Exp
  deriving (Eq, Ord, Show, Read)

newtype Environment = Env [Assignment]
  deriving (Eq, Ord, Show, Read)

-- State environment contains a counter and a hashmap in which the values 
-- track all the names that we are currently substituting the key for.
-- The last element in the sequence is the name to substitute the key for.
type IdMap = HashMap String [Int]
newtype IdMonad a = IdMonad {
    runId :: StateT Int (Reader IdMap) a
} deriving (    Functor, Applicative, Monad,
                MonadState Int,
                MonadReader IdMap   )

-- Returns an empty substitution list
emptySubL :: Raw.Environment
emptySubL = Raw.Env []

-- Returns a sublist with the freevars in some term
-- TODO: How do you get the free vars preemptively?
freeSubL :: Raw.Environment
freeSubL = Raw.Env []

-- Increments counter and pushes new substitute for x onto m[x]
pushVar :: Raw.Ident -> Int -> IdMap -> IdMap
pushVar (Raw.Ident x) c = HM.insertWith (++) x [c]

-- Pushes all variables in a substitution list
pushVars :: [Raw.Assignment] -> Int -> IdMap -> IdMap
pushVars l c m = do
    let idxd = Prelude.map (\(i,a) -> (i+c,a)) (indexed l)
    let push1 = \m1 (id, Raw.Assign x _ t) -> pushVar x id m1
    Prelude.foldl push1 m idxd

varAssign :: Raw.Assignment -> IdMonad Assignment
varAssign (Raw.Assign x _ t) = do
    cur <- modify (+1) >> get
    sub1 <- asks (getSub x . pushVar x cur)
    -- x is not bound in t
    Assign sub1 <$> reName t

-- Gets the latest substitute for x from m[x] (returns x if none are found)
getSub :: Raw.Ident -> IdMap -> Ident
getSub (Raw.Ident x) m = Ident x (head $ HM.findWithDefault [0] x m) 0

-- TODO check for faulty programs
reName :: Raw.Exp -> IdMonad Exp
reName exp = case exp of
    Raw.Var v           -> asks (Var . getSub v)
    Raw.Val v           -> return $ Val v
    Raw.BVal v          -> return $ BVal v
    Raw.Next e          -> Next <$> reName e
    Raw.PrevE e         -> reName $ Raw.Prev emptySubL e
    Raw.PrevF e         -> reName $ Raw.Prev freeSubL e
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

-- An identifier is free if it is at level 0
isFree :: Ident -> Bool
isFree (Ident _ 0 _)    = True
isFree _                = False
