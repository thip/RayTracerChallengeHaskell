module Matrices where

import Tuples

data Matrix4 = Matrix4 V4 V4 V4 V4 deriving (Eq, Show)
data Matrix3 = Matrix3 V3 V3 V3 deriving (Eq, Show)
data Matrix2 = Matrix2 V2 V2 deriving (Eq, Show)

data V4 = V4 Double Double Double Double deriving (Eq, Show)
data V3 = V3 Double Double Double deriving (Eq, Show)
data V2 = V2 Double Double deriving (Eq, Show)

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
  determinant _ = undefined

instance Matrixish Matrix3 where
  (Matrix3 r0 r1 r2) !? (a:b:[]) = index3 ([r0,r1,r2] !! a) b
  _ !? _ = undefined
  _ *|| _ = undefined
  transpose _ = undefined
  determinant _ = undefined

instance Matrixish Matrix2 where
  (Matrix2 r0 r1) !? (a:b:[]) = index2 ([r0,r1] !! a) b
  _ !? _ = undefined
  _ *|| _ = undefined
  transpose _ = undefined
  determinant m = ((m !? [0,0]) * (m !? [1,1])) - ((m !? [0,1]) * (m !? [1,0]))

index4 :: V4 -> Int -> Double
index4 (V4 a b c d) i = [a,b,c,d] !! i

index3 :: V3 -> Int -> Double
index3 (V3 a b c) i = [a,b,c] !! i

index2 :: V2 -> Int -> Double
index2 (V2 a b) i = [a,b] !! i

deleteIndex n xs = left ++ right
  where (left, (_:right)) = splitAt n xs

class Matrixish m where
  (!?) :: m -> [Int] -> Double
  (*||) :: m -> Tuple -> Tuple
  transpose :: m -> m
  determinant :: m -> Double

subMatrix4 :: Matrix4 -> Int -> Int -> Matrix3
subMatrix4 (Matrix4 r0 r1 r2 r3) row col = Matrix3 (rows !! 0) (rows !! 1) (rows !! 2)
    where rows = map dropCol (deleteIndex row [r0, r1, r2, r3])
          dropCol (V4 c0 c1 c2 c3) = V3 (cols !! 0) (cols !! 1) (cols !! 2)
            where cols = deleteIndex col [c0,c1,c2,c3]

subMatrix3 :: Matrix3 -> Int -> Int -> Matrix2
subMatrix3 (Matrix3 r0 r1 r2) row col = Matrix2 (rows !! 0) (rows !! 1)
    where rows = map dropCol (deleteIndex row [r0, r1, r2])
          dropCol (V3 c0 c1 c2) = V2 (cols !! 0) (cols !! 1)
            where cols = deleteIndex col [c0,c1,c2]

minor3 :: Matrix3 -> Int -> Int -> Double
minor3 m row col = determinant (subMatrix3 m row col)

minor4 :: Matrix4 -> Int -> Int -> Double
minor4 m row col = determinant3 (subMatrix4 m row col)

cofactor3 :: Matrix3 -> Int -> Int -> Double
cofactor3 m row col
  | odd (row + col) = negate minor
  | otherwise = minor
    where minor = minor3 m row col

cofactor4 :: Matrix4 -> Int -> Int -> Double
cofactor4 m row col
  | odd (row + col) = negate minor
  | otherwise = minor
    where minor = minor4 m row col

determinant3 :: Matrix3 -> Double
determinant3 m = sum [ partial x | x <- [0..2]]
  where partial col = (m !? [0,col]) * (cofactor3 m 0 col)

determinant4 :: Matrix4 -> Double
determinant4 m = sum [ partial x | x <- [0..3]]
  where partial col = (m !? [0,col]) * (cofactor4 m 0 col)

identity4 = Matrix4 (V4 1 0 0 0)
                    (V4 0 1 0 0)
                    (V4 0 0 1 0)
                    (V4 0 0 0 1)

invertable4 :: Matrix4 -> Bool
invertable4 m = 0 /= determinant4 m

inverse4 :: Matrix4 -> Matrix4
inverse4 m = Matrix4 (V4 (partial 0 0) (partial 0 1) (partial 0 2) (partial 0 3))
                     (V4 (partial 1 0) (partial 1 1) (partial 1 2) (partial 1 3))
                     (V4 (partial 2 0) (partial 2 1) (partial 2 2) (partial 2 3))
                     (V4 (partial 3 0) (partial 3 1) (partial 3 2) (partial 3 3))
  where partial row col = (cofactor4 m col row)/(determinant4 m)