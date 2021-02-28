{-# LANGUAGE CPP, ImplicitParams #-}
module Syntax.Fail where

import GHC.Stack

failure :: (Show a, ?loc :: CallStack) => a -> b
failure x = error $ "Undefined case: " ++ show x ++ "\n" ++ show ?loc
