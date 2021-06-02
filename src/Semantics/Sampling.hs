module Semantics.Sampling where

-- Probability density function of the gaussian distribution
pdfNorm :: [Double] -> Double -> Double
pdfNorm params c = do
    let m = head params
    let sd = sqrt ( params !! 1 )
    (1 / (sd * sqrt (2 * pi))) * exp (-0.5 * (((c - m) / sd) ^^ 2)) 

pdfRand :: [Double] -> Double -> Double
pdfRand params c = if 0 <= c && c <= 1 then 1 else 0

data Distribution = Distribution {
    name :: String,
    pdf :: [Double] -> Double -> Double
}

normalDist :: Distribution
normalDist = Distribution "Normal" pdfNorm

randDist :: Distribution
randDist = Distribution "Uniform random" pdfRand
