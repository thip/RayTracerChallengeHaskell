module Matrices2Spec (spec) where

import Test.Hspec
import SpecHelpers
import Tuples
import Matrices

spec :: Spec
spec = do
  describe "2x2 Matrices" $ do
    context "when constructed" $ do
      let m = Matrix2 (V2  (-3) 5   )
                      (V2  1    (-2))

      it "puts the elements in the right places" $ do
        m !? [0,0] `shouldBe` (-3)
        m !? [0,1] `shouldBe` 5
        m !? [1,0] `shouldBe` 1
        m !? [1,1] `shouldBe` (-2)

    context "when calculating the determinant" $ do
       let m = Matrix2 (V2  1    5 )
                       (V2  (-3) 2 )

       it "finds the difference between the products of the diagonals" $ do
         determinant(m) `shouldBe` 17

  describe "Matrix equality" $ do
    context "when two matrices are the same" $ do
      let m1 = Matrix4 (V4 1 2 3 4)
                       (V4 5 6 7 8)
                       (V4 9 8 7 6)
                       (V4 5 4 3 2)

      let m2 = Matrix4 (V4 1 2 3 4)
                       (V4 5 6 7 8)
                       (V4 9 8 7 6)
                       (V4 5 4 3 2)

      it "returns true for equality" $ do
        m1 == m2 `shouldBe` True

    context "when two matrices are not the same" $ do
      let m1 = Matrix4 (V4 1 2 3 4)
                       (V4 5 6 7 8)
                       (V4 9 8 7 6)
                       (V4 5 4 3 2)

      let m2 = Matrix4 (V4 2 3 4 5)
                       (V4 6 7 8 9)
                       (V4 8 7 6 5)
                       (V4 4 3 2 1)

      it "returns false for equality" $ do
        m1 == m2 `shouldBe` False


