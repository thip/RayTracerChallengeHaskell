module Matrices3Spec (spec) where

import Test.Hspec
import SpecHelpers
import Tuples
import Matrices

spec :: Spec
spec = do
  describe "3x3 Matrices" $ do
    context "when constructed" $ do
      let m = Matrix3 (V3  (-3) 5    0   )
                      (V3  1    (-2) (-7))
                      (V3  0    1    1   )
      
      it "puts the elements in the right places" $ do
        m !? [0,0] `shouldBe` (-3)
        m !? [1,1] `shouldBe` (-2)
        m !? [2,2] `shouldBe` 1

    context "sub matrix" $ do
      let m = Matrix3 (V3 1    5  3)
                      (V3 (-3) 2  7)
                      (V3 (0)  6 (-3))

      let sub = Matrix2 (V2 (-3) 2)
                        (V2 (0)  6)

      it "is a 2x2 matrix where the provided row and column of the original matrix have been removed" $ do
        subMatrix3 m 0 2 `shouldBe` sub

    context "minor" $ do
      let m = Matrix3 (V3 3   5   0)
                      (V3 2 (-1) (-7))
                      (V3 6 (-1)  5)

      it "is calculated correctly" $ do
        minor3 m 1 0 `shouldBe` 25

    context "cofactor" $ do
      let m = Matrix3 (V3 3   5   0)
                      (V3 2 (-1) (-7))
                      (V3 6 (-1)  5)

      it "is calculated correctly at 0 0" $ do
        cofactor3 m 0 0 `shouldBe` (-12)

      it "is calculated correctly at 1 0" $ do
        cofactor3 m 1 0 `shouldBe` (-25)

    context "determinant" $ do
      let m = Matrix3 (V3   1  2   6)
                       (V3 (-5) 8 (-4))
                       (V3   2  6   4)

      it "is calculated correctly" $ do
        determinant3(m) `shouldBe` (-196)