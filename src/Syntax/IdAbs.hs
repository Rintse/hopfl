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

data Ident = Ident String Integer Integer
  deriving (Eq, Ord, Show, Read)

data Exp
    = Var Ident
    | Val Double
    | BVal Raw.BConst
    | Next Exp
    | Box Raw.SubL Exp
    | Unbox Exp
    | Prev SubL Exp
    | PrevE Exp
    | PrevF Exp
    | In Exp
    | Out Exp
    | Fst Exp
    | Snd Exp
    | InL Exp
    | InR Exp
    | App Exp Exp
    | LApp Exp Raw.TLApp Exp
    | Mul Exp Exp
    | Div Exp Exp
    | Add Exp Exp
    | Sub Exp Exp
    | Eq Exp Exp
    | Lt Exp Exp
    | Gt Exp Exp
    | Leq Exp Raw.TLeq Exp
    | Geq Exp Raw.TGeq Exp
    | Not Raw.TNot Exp
    | And Exp Raw.Conj Exp
    | Or Exp Raw.Disj Exp
    | Pair Exp Exp
    | Norm Exp
    | Ite Exp Exp Exp
    | Match Exp Ident Exp Ident Exp
    | Abstr Raw.Lam Ident Exp
    | Rec Ident Exp
    deriving (Eq, Ord, Show, Read)

newtype SubL = SubList Environment
  deriving (Eq, Ord, Show, Read)

data Assignment = Assign Ident Exp
  deriving (Eq, Ord, Show, Read)

newtype Environment = Env [Assignment]
  deriving (Eq, Ord, Show, Read)

-- Get the ID from an assignment
getIDA :: Raw.Assignment -> Raw.Ident
getIDA (Raw.Assign x _ _) = x

-- Get the term from an assignment
getTermA :: Raw.Assignment -> Raw.Exp
getTermA (Raw.Assign _ _ t) = t

-- State environment contains a counter and a hashmap in which the values 
-- track all the names that we are currently substituting the key for.
-- The last element in the sequence is the name to substitute the key for.
type IdMap = HashMap String [Integer]
newtype IdMonad a = IdMonad {
    runId :: StateT Integer (Reader IdMap) a
} deriving (    Functor, Applicative, Monad,
                MonadState Integer,
                MonadReader IdMap   )

-- Returns an empty substitution list
emptySubL :: Raw.SubL
emptySubL = Raw.SubList $ Raw.Env []

-- Returns a sublist with the freevars in some term
-- TODO: How do you get the free vars preemptively?
freeSubL :: Raw.SubL
freeSubL = Raw.SubList $ Raw.Env []

-- Increments counter and pushes new substitute for x onto m[x]
pushVar :: Raw.Ident -> Integer -> IdMap -> IdMap
pushVar (Raw.Ident x) c = HM.insertWith (++) x [c]

-- Pushes all variables in a substitution list
pushVars :: Raw.SubL -> Integer -> IdMap -> IdMap
pushVars (Raw.SubList (Raw.Env l)) c m = Prelude.foldl f m l
    where f = \m1 x -> pushVar (getIDA x) c m1

-- Gets the latest substitute for x from m[x] (returns x if none are found)
getSub :: Raw.Ident -> IdMap -> Ident
getSub (Raw.Ident x) m = Ident x (head $ HM.findWithDefault [0] x m) 0

-- Gets all the subs from a substituion list
getSubs :: Raw.SubL -> IdMap -> [Ident]
getSubs (Raw.SubList (Raw.Env l)) m = Prelude.map f l
    where f = \(Raw.Assign x o t) -> getSub x m

-- Rename an entire list of substitutions
-- !!! Not bound by variables in sub list
reNameTermList :: Raw.SubL -> IdMonad [Exp]
reNameTermList (Raw.SubList (Raw.Env l)) = mapM f l
    where f = reName . getTermA

-- Merges the ids and terms from a renamed sublist
zipSubL :: [Ident] -> [Exp] -> SubL
zipSubL xs ts = SubList $ Env $ zipWith Assign xs ts

-- TODO check for faulty programs
reName :: Raw.Exp -> IdMonad Exp
reName exp = case exp of
    Raw.Var v           -> asks (Var . getSub v)
    Raw.Val v           -> return $ Val v
    Raw.BVal v          -> return $ BVal v
    Raw.Next e          -> Next <$> reName e
    Raw.PrevE e         -> reName $ Raw.Prev emptySubL e
    Raw.PrevF e         -> reName $ Raw.Prev freeSubL e
    Raw.Box l e         -> Box l <$> reName e
    Raw.In e            -> In <$> reName e
    Raw.Out e           -> Out <$> reName e
    Raw.App e1 e2       -> liftA2 App (reName e1) (reName e2)
    Raw.LApp e1 o e2    -> liftA3 LApp (reName e1) (return o) (reName e2)
    Raw.Pair e1 e2      -> liftA2 Pair (reName e1) (reName e2)
    Raw.Fst e           -> Fst <$> reName e
    Raw.Snd e           -> Snd <$> reName e
    Raw.InL e           -> InL <$> reName e
    Raw.InR e           -> InR <$> reName e
    Raw.Norm e          -> Norm <$> reName e
    Raw.Ite b e1 e2     -> liftA3 Ite (reName b) (reName e1) (reName e2)
    Raw.Prev l e        -> do
        cur <- modify (+1) >> get
        rl <- asks (getSubs l . pushVars l cur)
        rt <- reNameTermList l
        re <- local (pushVars l cur) $ reName e
        return $ Prev (zipSubL rl rt) re
    Raw.Match e x l y r -> do
        cur <- modify (+1) >> get
        re <- reName e
        rx <- asks (getSub x . pushVar x cur)
        rl <- local (pushVar x cur) $ reName l
        ry <- asks (getSub y . pushVar y cur)
        rr <- local (pushVar y cur) $ reName r
        return $ Match re rx rl ry rr
    Raw.Abstr l x e -> do
        cur <- modify (+1) >> get
        r1 <- asks (getSub x . pushVar x cur)
        r2 <- local (pushVar x cur) $ reName e
        return $ Abstr l r1 r2
    Raw.Rec x e -> do
        cur <- modify (+1) >> get
        r1 <- asks (getSub x . pushVar x cur)
        r2 <- local (pushVar x cur) $ reName e
        return $ Rec r1 r2
    Raw.Add e1 e2       -> liftA2 Add (reName e1) (reName e2)
    Raw.Sub e1 e2       -> liftA2 Sub (reName e1) (reName e2)
    Raw.Mul e1 e2       -> liftA2 Mul (reName e1) (reName e2)
    Raw.Div e1 e2       -> liftA2 Div (reName e1) (reName e2)
    Raw.And e1 o e2     -> liftA3 And (reName e1) (return o) (reName e2)
    Raw.Or e1 o e2      -> liftA3 Or (reName e1) (return o) (reName e2)
    Raw.Not n e         -> fmap (Not n) (reName e)
    Raw.Eq e1 e2        -> liftA2 Eq (reName e1) (reName e2)
    Raw.Lt e1 e2        -> liftA2 Lt (reName e1) (reName e2)
    Raw.Gt e1 e2        -> liftA2 Gt (reName e1) (reName e2)
    Raw.Leq e1 o e2     -> liftA3 Leq (reName e1) (return o) (reName e2)
    Raw.Geq e1 o e2     -> liftA3 Geq (reName e1) (return o) (reName e2)

-- Translate a raw tree into the id tree with annotated identifiers
idExp :: Raw.Exp -> Exp
idExp e = runReader (evalStateT (runId (reName e)) 0 ) HM.empty

-- An identifier is free if it is at level 0
isFree :: Ident -> Bool
isFree (Ident _ 0 _)    = True
isFree _                = False
