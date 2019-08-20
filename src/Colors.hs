module Colors where

import Tuples

data Color = Color { red :: Double,
                     green :: Double,
                     blue :: Double } deriving (Eq, Show)

instance Num Color where
  (Color r1 g1 b1) + (Color r2 g2 b2) = Color (r1+r2) (g1+g2) (b1+b2)
  (Color r1 g1 b1) - (Color r2 g2 b2) = Color (r1-r2) (g1-g2) (b1-b2)
  (Color r1 g1 b1) * (Color r2 g2 b2) = Color (r1*r2) (g1*g2) (b1*b2)
  negate (Color r g b) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined

instance Tuplish Color where
  (*|) (Color r g b) s = Color (r*s) (g*s) (b*s)
  (|*) s c = c *| s