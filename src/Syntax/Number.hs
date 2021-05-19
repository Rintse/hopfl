-- Datatype representing numbers. Deals with both integers and doubles
-- Defines common operations (and coercions) as well

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, LambdaCase #-}

module Syntax.Number where

import Data.Fixed

data Number = Whole Integer | Fract Double
    deriving (Read)

instance Show Number where
    show (Whole i) = show i
    show (Fract d) = show d

instance Num Number where
    fromInteger i = Whole i
    abs (Whole i) = Whole (abs i)
    abs (Fract d) = Fract (abs d)
    signum (Whole i) = Whole (signum i)
    signum (Fract d) = Fract (signum d)

    -- Addition
    Whole i1 + Whole i2 = Whole (i1 + i2)
    Fract d1 + Fract d2 = Fract (d1 + d2)
    Whole i1 + Fract d2 = Fract (fromIntegral i1 + d2)
    Fract d1 + Whole i2 = Fract (d1 + fromIntegral i2)

    -- Subtraction
    Whole i1 - Whole i2 = Whole (i1 - i2)
    Fract d1 - Fract d2 = Fract (d1 - d2)
    Whole i1 - Fract d2 = Fract (fromIntegral i1 - d2)
    Fract d1 - Whole i2 = Fract (d1 - fromIntegral i2)

    -- Multiplication
    Whole i1 * Whole i2 = Whole (i1 * i2)
    Fract d1 * Fract d2 = Fract (d1 * d2)
    Whole i1 * Fract d2 = Fract (fromIntegral i1 * d2)
    Fract d1 * Whole i2 = Fract (d1 * fromIntegral i2)
    
    -- Negation
    negate (Whole i) = Whole (negate i)
    negate (Fract d) = Fract (negate d)

instance Eq Number where
    Fract d1 == Fract d2 = d1 == d2
    Whole i1 == Whole i2 = i1 == i2
    Whole i1 == Fract d2 = fromIntegral i1 == d2
    Fract d1 == Whole i2 = d1 == fromIntegral i2

instance Ord Number where
    Fract d1 <= Fract d2 = d1 <= d2
    Whole i1 <= Whole i2 = i1 <= i2
    Whole i1 <= Fract d2 = fromIntegral i1 <= d2
    Fract d1 <= Whole i2 = d1 <= fromIntegral i2

numDiv :: Number -> Number -> Number
numDiv e1 e2 = case (e1,e2) of
    (Fract d1, Fract d2) -> Fract (d1 / d2)
    (Fract d1, Whole i2) -> Fract (d1 / fromIntegral i2)
    (Whole i1, Fract d2) -> Fract (fromIntegral i1 / d2)
    (Whole i1, Whole i2) -> Fract (fromIntegral i1 / fromIntegral i2)

numPow :: Number -> Number -> Number
numPow e1 e2 = case (e1,e2) of
    (Fract d1, Fract d2) -> Fract (d1 ** d2)
    (Fract d1, Whole i2) -> Fract (d1 ** fromIntegral i2)
    (Whole i1, Fract d2) -> Fract (fromIntegral i1 ** d2)
    (Whole i1, Whole i2) -> Whole (i1 ^ i2)

numMod :: Number -> Number -> Number
numMod e1 e2 = case (e1,e2) of
    (Fract d1, Fract d2) -> Fract (d1 `mod'` d2)
    (Fract d1, Whole i2) -> Fract (d1 `mod'` fromIntegral i2)
    (Whole i1, Fract d2) -> Fract (fromIntegral i1 `mod'` d2)
    (Whole i1, Whole i2) -> Whole (i1 `mod` i2)

