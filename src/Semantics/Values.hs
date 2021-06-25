-- Defines the values for the big-step semantics 
-- implemented in the Evaluation module

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, LambdaCase #-}

module Semantics.Values where

import Syntax.Expression
import Syntax.Number
import qualified Syntax.Raw.Abs as Raw

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

-- Result values
data Value
    = VSingle
    | VVal Number
    | VBVal Bool
    | VPair Exp Exp
    | VList [Exp]
    | VIn Exp
    | VInL Exp
    | VInR Exp
    | VNext Exp
    | VBox Environment Exp
    | VOut Exp
    | VThunk Exp
    
    -- Unevaluated next
    | VUNext Exp
    -- Evaluated results
    | VEPair Value Value 
    | VEBox Value
    | VENext Value
    | VEIn Value
    | VEInL Value
    | VEInR Value
    deriving (Eq, Ord, Show, Read)

makeBaseFunctor ''Value

fromBool :: Bool -> Raw.BConst
fromBool b = if b then Raw.BTrue else Raw.BFalse
toBool :: Raw.BConst -> Bool
toBool = \case
    Raw.BTrue -> True
    Raw.BFalse -> False

toExp :: Value -> Exp
toExp (VVal v) = Val v
toExp (VBVal v) = BVal (fromBool v)
toExp (VIn e) = In e
toExp (VInL e) = InL e
toExp (VInR e) = InR e
toExp (VThunk e) = e
toExp (VPair e1 e2) = Pair e1 e2
toExp (VList l) = List l
toExp (VNext e) = Next e
toExp (VOut e) = Out e
toExp (VBox l e) = Box l e

toExp (VEPair e1 e2) = Pair (toExp e1) (toExp e2)
toExp (VEInL e) = InL $ toExp e
toExp (VEInR e) = InR $ toExp e

-- Should never be reached
toExp e = BVal Raw.BFalse
