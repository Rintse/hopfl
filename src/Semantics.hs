{-# LANGUAGE FlexibleContexts #-}
-- Small step semantics for guarded HOPFL
module Semantics where

import Syntax.Abs
import Syntax.ErrM
import Syntax.Fail

import Data.HashMap.Lazy as HM
import Control.Monad.Reader
import Control.Applicative

data Value
  = VDouble Double
  | VPair Value Value
  | VThunk Exp
  deriving (Eq, Show)

instance Ord Value where
  VDouble m <= VDouble n = m <= n
  _ <= _ = False

toExp :: Value -> Exp
toExp (VDouble v) = Val v
toExp (VThunk e) = e

-- Environment as hashmap
type Env = HashMap String Exp

emptyEnv :: Env
emptyEnv = HM.empty

mkEnv :: Environment -> Env
mkEnv (Env e) = fromList $ fmap mkAssign e
  where
    mkAssign (Assign (Ident x) exp) = (x, exp)

-- Update the environment
update :: String -> Exp -> Env -> Env
update = insert

-- Lookup in the environment
getVal :: String -> Env -> Exp
getVal x e = e ! x


-- TODO: do not include tokens into the datatypes
evalExp :: MonadReader Env m => [Double] -> Exp -> m Value
evalExp s exp = case exp of
    Var (Ident v)   -> asks (getVal v) >>= evalExp s
    Val v           -> return $ VDouble v
    
    Next e          -> failure exp
    In e            -> failure exp
    Out e           -> failure exp
    App e1 e2       -> failure exp
    LApp e1 _ e2    -> failure exp
    
    Pair e1 e2 -> do
        r1 <- evalExp s e1
        r2 <- evalExp s e2
        return $ VPair r1 r2
    
    Fst e -> do
        r <- evalExp s e
        case r of
            VPair v1 v2 -> return v1
            _           -> error $ "Took fst of non-pair " ++ show r

    Snd e -> do
        r <- evalExp s e
        case r of
            VPair v1 v2 -> return v1
            _           -> error $ "Took snd of non-pair " ++ show r
    
    Norm e -> do
        r <- evalExp s e
        return $ VDouble 1.0
    
    a@(Abstr l x e) -> return $ VThunk a
    a@(Rec x e)     -> failure exp
    Typed e t       -> evalExp s e

