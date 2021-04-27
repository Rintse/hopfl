{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, LambdaCase #-}

module Syntax.AbsF where

import Syntax.Abs

import Data.Functor.Foldable.TH
import Data.Functor.Foldable

makeBaseFunctor ''Exp
