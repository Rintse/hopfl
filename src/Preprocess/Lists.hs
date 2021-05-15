{-# LANGUAGE LambdaCase #-}

module Preprocess.Lists where

import Syntax.Raw.Abs
import Syntax.AbsF

import Data.Functor.Foldable
import Data.Functor.Foldable.Monadic

-- A list is repeated pairing in InR followed by the singleton in InL
desugarList :: [El] -> Exp
desugarList l = do
    let end = In $ InL $ Single $ TSingle ""
    BoxI $ foldr ((\x y -> In $ InR $ Pair x $ Next y) . (\(Elem e) -> e)) end l

-- Desugar lists
desugarLists :: Exp -> IO Exp
desugarLists = anaM $ \case
    EList (List (Elems l)) -> return $ project $ desugarList l
    other -> return $ project other

