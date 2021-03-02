-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax.Abs where

import Prelude (Char, Double, Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype Lam = Lam String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype Mu = Mu String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype Prod = Prod String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype To = To String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype Later = Later String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype Lapp = Lapp String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

data Typ
    = TReal
    | TVar Ident
    | TLat Later Typ
    | TPRod Typ Prod Typ
    | TRec Mu Ident Typ
    | TFun Typ To Typ
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Exp
    = Var Ident
    | Val Double
    | Next Exp
    | In Exp
    | Out Exp
    | App Exp Exp
    | LApp Exp Lapp Exp
    | Pair Exp Exp
    | Fst Exp
    | Snd Exp
    | Norm Exp
    | Abstr Lam Exp Exp
    | Rec Exp Exp
    | Typed Exp Typ
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Assignment = Assign Ident Exp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Environment = Env [Assignment]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

