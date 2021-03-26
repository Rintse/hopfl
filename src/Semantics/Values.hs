module Semantics.Values where

import Syntax.Abs

-- Result values
data Value
  = VVal Double
  | VBVal Bool
  | VPair Exp Exp
  | VIn Exp
  | VInL Exp
  | VInR Exp
  | VNext Exp
  | VOut Exp
  | VThunk Exp
  deriving (Eq, Show)

fromBool :: Bool -> BConst
fromBool b = if b then BTrue else BFalse
toBool :: BConst -> Bool
toBool b = case b of
    BTrue -> True
    BFalse -> False

toExp :: Value -> Exp
toExp (VVal v) = Val v
toExp (VBVal v) = BVal (fromBool v)
toExp (VIn e) = In e
toExp (VInL e) = InL e
toExp (VInR e) = InR e
toExp (VThunk e) = e
toExp (VPair e1 e2) = Pair e1 e2
toExp (VNext e) = Next e
toExp (VOut e) = Out e

