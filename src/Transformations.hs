module Transformations where

import Matrices

translation :: Double -> Double -> Double -> Matrix4
translation x y z = Matrix4 (V4 1 0 0 x)
                            (V4 0 1 0 y)
                            (V4 0 0 1 z)
                            (V4 0 0 0 1)