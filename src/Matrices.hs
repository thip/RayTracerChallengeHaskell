module Matrices where

data Matrix4 = Matrix4 V4 V4 V4 V4 deriving Eq
data Matrix3 = Matrix3 V3 V3 V3 deriving Eq
data Matrix2 = Matrix2 V2 V2 deriving Eq

data V4 = V4 Float Float Float Float deriving Eq
data V3 = V3 Float Float Float deriving Eq
data V2 = V2 Float Float deriving Eq

instance Matrixish Matrix4 where
  (Matrix4 r0 r1 r2 r3) !? (a:b:[]) = index4 ([r0,r1,r2,r3] !! a) b
  _ !? _ = undefined

instance Matrixish Matrix3 where
  (Matrix3 r0 r1 r2) !? (a:b:[]) = index3 ([r0,r1,r2] !! a) b
  _ !? _ = undefined

instance Matrixish Matrix2 where
  (Matrix2 r0 r1) !? (a:b:[]) = index2 ([r0,r1] !! a) b
  _ !? _ = undefined

index4 :: V4 -> Int -> Float
index4 (V4 a b c d) i = [a,b,c,d] !! i

index3 :: V3 -> Int -> Float
index3 (V3 a b c) i = [a,b,c] !! i

index2 :: V2 -> Int -> Float
index2 (V2 a b) i = [a,b] !! i

class Matrixish m where
  (!?) :: m -> [Int] -> Float