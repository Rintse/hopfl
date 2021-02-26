-- Small step semantics for guarded HOPFL
module Semantics where

import Syntax.Abs
import Data.HashMap.Lazy as HM

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



result :: IO ()
result = print $ evalExp 0

evalExp :: Integer -> Float
evalExp x = 0 
