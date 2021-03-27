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


type IdMap = HashMap String [Integer]

-- State environment contains a counter and a hashmap in which the values 
-- track all the names that we are currently substituting the key for.
-- The last element in the sequence is the name to substitute the key for.
newtype IdMonad a = IdMonad {
    runId :: StateT Integer (Reader IdMap) a
} deriving (    Functor, Applicative, Monad, 
                MonadState Integer, 
                MonadReader IdMap   )

-- Increments counter and pushes new substitute for x onto m[x]
pushVar :: Raw.Ident -> Integer -> IdMap -> IdMap
pushVar (Raw.Ident x) c = HM.insertWith (++) x [c]

-- Gets the latest substitute for x from m[x] (returns x if none are found)
getSub :: Raw.Ident -> IdMap -> Ident
getSub v@(Raw.Ident x) m = Ident x (head $ HM.findWithDefault [0] x m) 0

-- TODO check for faulty programs
reName :: Raw.Exp -> IdMonad Exp
reName exp = case exp of
    Raw.Var v           -> asks (Var . getSub v)
    Raw.Val v           -> return $ Val v
    Raw.BVal v          -> return $ BVal v
    Raw.Next e          -> Next <$> reName e
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
