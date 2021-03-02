module Substitution where

import Control.Monad.Reader
import Data.Bifunctor

import Syntax.Abs


markVars :: Exp -> Exp
markVars e = snd $ markTree (0, e)

markTree :: (Integer, Exp) -> (Integer, Exp)
markTree (i, e) = case e of
    Var (Ident x)   -> (i+1, Var $ Ident (x ++ show i))
    Val v           -> (i, Val v)
    Next e          -> second Next (markTree (i, e))
    In e            -> second In (markTree (i, e))
    Out e           -> second Out (markTree (i, e))
    App e1 e2       -> let r1 = markTree (i, e1) in
                       let r2 = markTree (fst r1, e2) in
                       second (App (snd r1)) r2
    LApp e1 _ e2    -> let r1 = markTree (i, e1) in
                       let r2 = markTree (fst r1, e2) in
                       second (App (snd r1)) r2
    Pair e1 e2      -> let r1 = markTree (i, e1) in
                       let r2 = markTree (fst r1, e2) in
                       second (App (snd r1)) r2
    Fst e           -> second Fst (markTree (i, e))
    Snd e           -> second Snd (markTree (i, e))
    Norm e          -> second Norm (markTree (i, e))
    Abstr l x e     -> let r1 = markTree (i, x) in
                       let r2 = markTree (fst r1, e) in
                       (fst r2, Abstr l (snd r1) (snd r2))
    -- Rec x e         -> let r1 = markTree (i, x) in
                       -- let r2 = markTree (fst r1, e) in
                       -- (fst r2, Rec (snd r1) (snd r2))
    Typed e t       -> (i, Typed (snd $ markTree (i, e)) t)

insertBinds :: Exp -> Exp
insertBinds e = e

markBounded :: Exp -> String -> Exp
markBounded e x = case e of
    Var (Ident v)   -> Var $ Ident (if v == x then "$" ++ x else x)

removeBinder :: Exp -> Exp -> Exp
removeBinder e s = case e of
    Abstr l x e -> e
    Rec x e     -> e
    e           -> e


