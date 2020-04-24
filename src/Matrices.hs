module Matrices where

import Tuples

data Matrix4 = Matrix4 V4 V4 V4 V4 deriving (Eq, Show)
data Matrix3 = Matrix3 V3 V3 V3 deriving Eq
data Matrix2 = Matrix2 V2 V2 deriving Eq

data V4 = V4 Double Double Double Double deriving (Eq, Show)
data V3 = V3 Double Double Double deriving Eq
data V2 = V2 Double Double deriving Eq

instance Num Matrix4 where
  m1 * m2 = Matrix4 (V4 (p 0 0) (p 0 1) (p 0 2) (p 0 3))
                    (V4 (p 1 0) (p 1 1) (p 1 2) (p 1 3))
                    (V4 (p 2 0) (p 2 1) (p 2 2) (p 2 3))
                    (V4 (p 3 0) (p 3 1) (p 3 2) (p 3 3))
    where p row column = sum [((m1 !? [row, position]) * (m2 !? [position,column])) | position <- [0..3]]
  (+) = undefined
  (-) = undefined
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined

instance Matrixish Matrix4 where
  (Matrix4 r0 r1 r2 r3) !? (a:b:[]) = index4 ([r0,r1,r2,r3] !! a) b
  _ !? _ = undefined
  m *|| (Tuple x y z w) = Tuple (p 0) (p 1) (p 2) (p 3)
    where p row = sum [((m !? [row, position]) * ([x, y, z, w] !! position)) | position <- [0..3]]
  transpose m = Matrix4 (V4 (m !? [0,0]) (m !? [1,0]) (m !? [2,0]) (m !? [3,0]))
                        (V4 (m !? [0,1]) (m !? [1,1]) (m !? [2,1]) (m !? [3,1]))
                        (V4 (m !? [0,2]) (m !? [1,2]) (m !? [2,2]) (m !? [3,2]))
                        (V4 (m !? [0,3]) (m !? [1,3]) (m !? [2,3]) (m !? [3,3]))

instance Matrixish Matrix3 where
  (Matrix3 r0 r1 r2) !? (a:b:[]) = index3 ([r0,r1,r2] !! a) b
  _ !? _ = undefined
  _ *|| _ = undefined
  transpose _ = undefined

instance Matrixish Matrix2 where
  (Matrix2 r0 r1) !? (a:b:[]) = index2 ([r0,r1] !! a) b
  _ !? _ = undefined
  _ *|| _ = undefined
  transpose _ = undefined

index4 :: V4 -> Int -> Double
index4 (V4 a b c d) i = [a,b,c,d] !! i

index3 :: V3 -> Int -> Double
index3 (V3 a b c) i = [a,b,c] !! i

index2 :: V2 -> Int -> Double
index2 (V2 a b) i = [a,b] !! i

class Matrixish m where
  (!?) :: m -> [Int] -> Double
  (*||) :: m -> Tuple -> Tuple
  transpose :: m -> m

identity4 = Matrix4 (V4 1 0 0 0)
                    (V4 0 1 0 0)
                    (V4 0 0 1 0)
                    (V4 0 0 0 1)