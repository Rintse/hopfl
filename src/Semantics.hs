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
  = VReal Double
  | VPair Value Value
  | VThunk Exp
  deriving (Eq, Show)

instance Ord Value where
  VReal m <= VReal n = m <= n
  _ <= _ = False

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

evalExp :: MonadReader Env m => Exp -> m Value
evalExp exp = case exp of
    Val v   -> return $VReal v
    _       -> failure exp 
