module Substitution where

import Control.Monad.Reader
import Control.Applicative
import Control.Monad.State
import Data.HashMap.Lazy as HM
import Data.Bifunctor
import Data.Char
import Data.Sequence as Seq
import Debug.Trace

import Syntax.Abs

-- Marks each variable with its depth (w.r.t. binders, i.e. lambda, fix)
-- to avoid having to rename during substition. Variable names are prepended
-- with their depth when bound
-- NOTE: The resulting name is illegal, preventing collisions
type MarkMap = (Integer, HashMap String (Seq String))

markVars :: Exp -> Exp
markVars e = evalState (markSub2 e) (0, HM.empty)

-- Reader environment contains a hashmap that tracks
-- how many abstractions using the same variable deep we are

-- Adds a new variable to the environment (or updates existing)
-- pushVar :: Exp -> HashMap String Integer -> HashMap String Integer
-- pushVar (Var (Ident x)) = insertWith (+) x 1

-- popVar :: Exp -> HashMap  String Integer -> HashMap  String Integer
-- popVar (Var (Ident x)) = adjust (subtract 1) x

-- rename :: Exp -> HashMap String (Seq String) -> Exp
-- rename (Var (Ident x)) (c, _:|>l) = Var $ Ident (if snd l == x then x ++ show else x)

pushVar :: Exp -> MarkMap -> MarkMap
pushVar (Var (Ident x)) (c, m) = 
    let toAdd = Seq.singleton (x ++ show (c+1)) in
        (c+1, HM.insertWith (flip (><)) x toAdd m)

popVar :: Exp -> MarkMap -> MarkMap
popVar (Var (Ident x)) (c, m) = (c, HM.adjust (\(t:|>s) -> t) x m)

getSub :: Exp -> MarkMap -> Exp
getSub (Var (Ident x)) (c, m) = Var $ Ident (case HM.lookup x m of
    Just (_:|>s) -> s
    _            -> x) 

markSub2 :: Exp -> State MarkMap Exp
markSub2 e = case e of
    Var v           -> gets (getSub e)
    Val v           -> return $ Val v
    Next e          -> Next <$> markSub2 e
    In e            -> In <$> markSub2 e
    Out e           -> Out <$> markSub2 e
    App e1 e2       -> liftA2 App (markSub2 e1) (markSub2 e2)
    LApp e1 o e2    -> liftA3 LApp (markSub2 e1) (return o) (markSub2 e2)
    Pair e1 e2      -> liftA2 Pair (markSub2 e1) (markSub2 e2)
    Fst e           -> Fst <$> markSub2 e
    Snd e           -> Snd <$> markSub2 e
    Norm e          -> Norm <$> markSub2 e
    Abstr l x e     -> do
        modify (pushVar x)
        r1 <- markSub2 x
        r2 <- markSub2 e
        modify (popVar x)
        return $ Abstr l r1 r2
    Rec x e         -> do
        modify $ pushVar x
        r1 <- markSub2 x
        r2 <- markSub2 e
        modify (popVar x)
        return $ Rec r1 r2
    Typed e t       -> Typed <$> markSub2 e <*> return t


-- Checks whether a variable is free
isFree :: Exp -> Bool
isFree (Var (Ident e)) = not (isDigit $ head e)

-- Strips the marked variable name by removing the leading $ or digits
stripName :: String -> String
stripName = dropWhile isDigit

-- Prerequesite: The program tree is marked using markVars
-- Substitutes, in exp, x for s
substitute :: Exp -> String -> Exp -> Exp
substitute exp x s = case exp of
    Var (Ident v)   -> if v == x then s else exp
    Val v           -> exp
    Next e          -> Next $ substitute e x s
    In e            -> In $ substitute e x s
    Out e           -> Out $ substitute e x s
    App e1 e2       -> App (substitute e1 x s) (substitute e2 x s)
    LApp e1 o e2    -> LApp (substitute e1 x s) o (substitute e2 x s)
    Pair e1 e2      -> Pair (substitute e1 x s) (substitute e2 x s)
    Fst e           -> Fst $ substitute e x s
    Snd e           -> Snd $ substitute e x s
    Norm e          -> Norm $ substitute e x s
    Abstr l v e     -> Abstr l v (substitute e x s)
    Rec v e         -> Rec v (substitute e x s)
    Typed e t       -> Typed (substitute e x s) t

-- Removes a binder and substitutes the bound occurances
-- Used for performing application and recursion
removeBinder :: Exp -> Exp -> Exp
removeBinder exp s = case exp of
    Abstr l (Var (Ident x)) e   -> substitute e x s
    Rec (Var (Ident x)) e       -> substitute e x exp
    _ -> exp


