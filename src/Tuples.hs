module Tuples where

data Tuple = Tuple { x :: Double,
                     y :: Double,
                     z :: Double,
                     w :: Double } deriving (Eq, Show)

isPoint :: Tuple -> Bool
isPoint (Tuple _ _ _ 1.0) = True
isPoint (Tuple _ _ _ _) = False

isVector :: Tuple -> Bool
isVector (Tuple _ _ _ 0.0) = True
isVector (Tuple _ _ _ _) = False

point :: Double -> Double -> Double -> Tuple
point x y z = Tuple x y z 1.0

vector :: Double -> Double -> Double -> Tuple
vector x y z = Tuple x y z 0.0

(/|) :: Tuple -> Double -> Tuple
(/|) (Tuple x y z w) s = Tuple (x/s) (y/s) (z/s) (w/s)

magnitude :: Tuple -> Double
magnitude (Tuple x y z w) = sqrt $ x**2 + y**2 + z**2

normalize :: Tuple -> Tuple
normalize tuple = tuple /| magnitude tuple

dot :: Tuple -> Tuple -> Double
(Tuple x1 y1 z1 w1) `dot` (Tuple x2 y2 z2 w2) = (x1 * x2) + (y1 * y2) + (z1 * z2) + (w1 * w2)

cross :: Tuple -> Tuple -> Tuple
(Tuple x1 y1 z1 0) `cross` (Tuple x2 y2 z2 0) = Tuple (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2) 0
_ `cross` _ = undefined

instance Num Tuple where
 (Tuple x1 y1 z1 w1) + (Tuple x2 y2 z2 w2) = Tuple (x1+x2) (y1+y2) (z1+z2) (w1+w2)
 (Tuple x1 y1 z1 w1) - (Tuple x2 y2 z2 w2) = Tuple (x1-x2) (y1-y2) (z1-z2) (w1-w2)
 negate (Tuple x y z w) = (Tuple (-x) (-y) (-z) (-w))
 (*) = undefined
 abs = undefined
 signum = undefined
 fromInteger = undefined

instance Tuplish Tuple where
  (*|) (Tuple x y z w) s = Tuple (x*s) (y*s) (z*s) (w*s)
  (|*) s t = t *| s

class Tuplish t where
  (*|) :: t -> Double -> t
  (|*) :: Double -> t -> t

