{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, LambdaCase #-}

module Syntax.Expression where

import qualified Syntax.Raw.Abs as Raw
import Syntax.Number

import Data.Functor.Foldable.TH

-- Better expression data type that removes unnecessary data, desugars
-- some expressions, and allows variables to be annotated with identifiers
data Exp
    = Single
    | Var   Ident
    | Val   Number
    | BVal  Raw.BConst
    | Next  Exp
    | Unbox Exp
    | In    Exp
    | Out   Exp
    | Fst   Exp
    | Snd   Exp
    | InL   Exp
    | InR   Exp
    | Norm  Exp
    | Not   Exp
    | Min   Exp
    | FList Exp
    | App   Exp Exp
    | LApp  Exp Exp
    | Pow   Exp Exp
    | Mul   Exp Exp
    | Div   Exp Exp
    | Add   Exp Exp
    | Sub   Exp Exp
    | Eq    Exp Exp
    | Lt    Exp Exp
    | Gt    Exp Exp
    | Leq   Exp Exp
    | Geq   Exp Exp
    | And   Exp Exp
    | Or    Exp Exp
    | Pair  Exp Exp
    | Abstr Ident Exp
    | Rec   Ident Exp
    | Ite   Exp Exp Exp
    | Box   Environment Exp
    | Prev  Environment Exp
    | Match Exp Ident Exp Ident Exp
    deriving (Eq, Ord, Show, Read)

data Ident = Ident String Int Int
    deriving (Eq, Ord, Show, Read)

data Assignment = Assign Ident Exp
  deriving (Eq, Ord, Show, Read)

newtype Environment = Env [Assignment]
  deriving (Eq, Ord, Show, Read)

-- Make basefunctor to be able to use recursion schemes
makeBaseFunctor ''Exp
