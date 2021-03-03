{-# LANGUAGE FlexibleContexts #-}
-- Small step semantics for guarded HOPFL
module Semantics where

import Substitution
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

-- Transform the environment AST into a hashmap
mkEnv :: Environment -> Env
mkEnv (Env e) = fromList $ fmap mkAssign e
  where mkAssign (Assign (Ident x) exp) = (x, exp)

-- Update the environment
update :: String -> Exp -> Env -> Env
update = insert

-- Lookup in the environment
getVal :: String -> Env -> Exp
getVal x e = e ! x

pdfNorm :: (Double, Double) -> Double -> Double
pdfNorm (x, s) c = 1 / exp(((c-x) ^^ 2) / 2 * s) * sqrt(2 * s * pi)

-- TODO: do not include tokens into the datatypes
evalExp :: MonadReader Env m => [Double] -> Exp -> m Value
evalExp s exp = case exp of
    Var (Ident v)   -> asks (getVal v) >>= evalExp s
    Val v           -> return $ VDouble v
    
    Next e          -> failure exp
    In e            -> failure exp
    Out e           -> failure exp
    App e1 e2       -> do
        r1 <- evalExp s e1
        r2 <- evalExp s e2
        case r1 of
            VThunk e    -> evalExp s (removeBinder (toExp r1) (toExp r2))
            _           -> error $ show r1 ++ " is not a function"

    LApp e1 _ e2    -> failure exp
    
    Pair e1 e2 -> VPair <$> evalExp s e1 <*> evalExp s e2
    
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
        case r of
            VPair (VDouble mean) (VDouble dev) -> 
                return $ VDouble $ pdfNorm (mean, dev) 1.0
            _ -> error "Normal argument not a pair of real numbers"
    
    Abstr l x e     -> return $ VThunk exp
    Rec x e         -> failure exp
    Typed e t       -> evalExp s e

