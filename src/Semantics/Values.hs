module Semantics.Values where

import Syntax.IdAbs
import qualified Syntax.Abs as Raw

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

fromBool :: Bool -> Raw.BConst
fromBool b = if b then Raw.BTrue else Raw.BFalse
toBool :: Raw.BConst -> Bool
toBool b = case b of
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
toExp (VNext e) = Next e
toExp (VOut e) = Out e

