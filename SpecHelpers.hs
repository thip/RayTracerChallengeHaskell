module SpecHelpers where

isApproximately :: (Fractional n, Ord n) => n -> n -> Bool
isApproximately a b =  0.000001 > abs(a - b)