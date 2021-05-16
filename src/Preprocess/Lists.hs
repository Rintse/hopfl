{-# LANGUAGE LambdaCase #-}

module Preprocess.Lists where

import Syntax.Raw.Abs
import Syntax.AbsF

import Data.Functor.Foldable
import Data.Functor.Foldable.Monadic

lEmpty :: Exp
lEmpty = In $ InL $ Single $ TSingle ""

lCons :: El -> Exp -> Exp
lCons (Elem e) l = In $ InR $ Pair e $ Next l

-- A list is repeated pairing in InR followed by the singleton in InL
desugarList :: [El] -> Exp
desugarList l = BoxI $ foldr lCons lEmpty l

-- Desugar lists
desugarLists :: Exp -> IO Exp
desugarLists = anaM $ \case
    EList (List (Elems l)) -> return $ project $ desugarList l
    other -> return $ project other

