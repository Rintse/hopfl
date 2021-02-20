module Semantics
    ( result
    ) where

result :: IO ()
result = print $ eval 0

eval :: Integer -> Float
eval x = 0 
