module Syntax.Substitute where

import Syntax.Abs

-- Substitutes x for s in e
sub :: Exp -> String -> Exp -> Exp
sub e x s = Var $ Ident "lel"
