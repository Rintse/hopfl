
module Semantics.Tools where

import Syntax.IdAbs
import qualified Syntax.Abs as Raw
import Syntax.ErrM
import Semantics.Values

import Data.HashMap.Lazy as HM
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

-- Environment as hashmap from names to values
type Env = HashMap String Exp

-- Transform the environment AST into a hashmap
mkEnv :: Raw.Environment -> Env
mkEnv (Raw.Env e) = fromList $ fmap mkAssign e
    where mkAssign (Raw.Assign (Raw.Ident x) exp) = (x, idExp exp)

printEnv :: Env -> String
printEnv m = show $ Prelude.map (\x -> 
    fst x ++ "=" ++ case snd x of 
        Val v -> show v
    ) (toList m)

-- Probability density function of the gaussian distribution
pdfNorm :: (Double, Double) -> Double -> Double
pdfNorm (m,v) c = let sd = sqrt v in -- variance is σ^2
    (1 / (sd * sqrt (2 * pi))) * exp (-0.5 * (((c - m) / sd) ^^ 2)) 


