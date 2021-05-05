{-# LANGUAGE LambdaCase #-}

module Preprocess.Lists where

import Syntax.Raw.Abs
import Syntax.AbsF

import Data.Functor.Foldable

-- A list is repeated pairing in InR followed by the singleton in InL
desugarList :: [El] -> Exp
desugarList l = do
    let end = InL $ Single $ TSingle ""
    foldr ((\x y -> InR $ Pair x y) . (\(Elem e) -> e)) end l

-- Desugar lists
desugarLists :: Exp -> Exp
desugarLists = ana $ \case
    EList (List (Elems l)) -> project $ desugarList l
    other -> project other

