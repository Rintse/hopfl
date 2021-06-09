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
    -- Base types
    | Var   Ident
    | Val   Number
    | BVal  Raw.BConst

    -- Lambda calculus with ite
    | App   Exp Exp
    | Abstr Ident Exp
    | Ite   Exp Exp Exp

    -- Products
    | Fst   Exp
    | Snd   Exp
    | Pair  Exp Exp

    -- Coproducts
    | InL   Exp
    | InR   Exp
    | Match Exp Ident Exp Ident Exp
    
    -- Guarded recursion
    | Next  Exp
    | DApp  Exp Exp
    | Rec   Ident Exp
    -- Iso recursive types
    | In    Exp
    | Out   Exp

    -- Constant modality
    | Unbox Exp
    | Box   Environment Exp
    | Prev  Environment Exp

    -- Sampling
    | Rand
    | Norm  Exp

    --Logic
    | Not   Exp
    | And   Exp Exp
    | Or    Exp Exp

    -- Relative operators
    | Eq    Exp Exp
    | Lt    Exp Exp
    | Gt    Exp Exp
    | Leq   Exp Exp
    | Geq   Exp Exp

    -- Arithmetic
    | Min   Exp
    | Force Exp
    | Pow   Exp Exp
    | Mul   Exp Exp
    | Mod   Exp Exp
    | Div   Exp Exp
    | Add   Exp Exp
    | Sub   Exp Exp
    
    -- List basics
    | List      [ Exp ]
    | LCons     Exp Exp
    | LAppend   Exp Exp
    | LIndex    Exp Exp
    | LHead     Exp
    | LTail     Exp
    | LNull     Exp
    | LLength   Exp
    
    -- List functions
    | LFold     Exp Exp Exp
    | LMap      Exp Exp
    | LElem     Exp Exp
    | LTake     Exp Exp
    | LDrop     Exp Exp
    deriving (Eq, Ord, Show, Read)

data Ident = Ident String Int Int
    deriving (Eq, Ord, Show, Read)

data Assignment = Assign Ident Exp
  deriving (Eq, Ord, Show, Read)

newtype Environment = Env [Assignment]
  deriving (Eq, Ord, Show, Read)

-- Make basefunctor to be able to use recursion schemes
makeBaseFunctor ''Exp
