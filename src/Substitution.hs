module Substitution where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor

import Syntax.Abs

markVars :: Exp -> Exp
markVars e = evalState (markSub e) 0 

markSub :: Exp -> State Integer Exp
markSub e = case e of
    Var (Ident x)   -> modify (+1) >> gets (\l -> Var $ Ident (x ++ show l))
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
    Abstr l x e     -> Abstr l <$> markSub x <*> markSub e
    Rec x e         -> Rec <$> markSub x <*> markSub e
    Typed e t       -> Typed <$> markSub e <*> return t



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


