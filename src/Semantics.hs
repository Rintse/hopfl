-- Small step semantics for guarded HOPFL
module Semantics where

import Data.HashMap.Lazy as HM
import DeBruijn.Abs as DB
import DeBruijn.Translate
import DeBruijn.Substitution

-- Environment as hashmap
type Env = HashMap String DB.Exp

emptyEnv :: Env
emptyEnv = HM.empty

mkEnv :: Raw.Environment -> Env
mkEnv (Raw.Env e) = fromList $ fmap mkAssign e
  where
    mkAssign (Raw.Assign (Raw.Ident x) exp) = (x, toDeBruijnTree exp)

-- Update the environment
update :: String -> DB.Exp -> Env -> Env
update = insert

-- Lookup in the environment
getVal :: String -> Env -> DB.Exp
getVal x e = e ! x



result :: IO ()
result = print $ eval 0

eval :: Integer -> Float
eval x = 0 
