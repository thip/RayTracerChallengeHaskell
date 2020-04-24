module MatricesSpec (spec) where

import Test.Hspec
import SpecHelpers
import Tuples
import Matrices

spec :: Spec
spec = do
  describe "4x4 Matrices" $ do
    context "when constructed" $ do
      let m = Matrix4 (V4  1    2    3    4   )
                      (V4  5.5  6.5  7.5  8.5 )
                      (V4  9    10   11   12  )
                      (V4  13.5 14.5 15.5 16.5)
      
      it "puts the elements in the right places" $ do
        m !? [0,0] `shouldBe` 1
        m !? [0,3] `shouldBe` 4
        m !? [1,0] `shouldBe` 5.5
        m !? [1,2] `shouldBe` 7.5
        m !? [2,2] `shouldBe` 11
        m !? [3,0] `shouldBe` 13.5
        m !? [3,2] `shouldBe` 15.5

    context "when multiplied by other 4x4 matrices" $ do
      let m1 = Matrix4 (V4 1 2 3 4)
                       (V4 5 6 7 8)
                       (V4 9 8 7 6)
                       (V4 5 4 3 2)

      let m2 = Matrix4 (V4 (-2) 1 2 3)
                       (V4 3 2 1 (-1))
                       (V4 4 3 6 5)
                       (V4 1 2 7 8)

      let m1TimesM2 = Matrix4 (V4 20 22 50 48)
                              (V4 44 54 114 108)
                              (V4 40 58 110 102)
                              (V4 16 26 46 42)

      it "should produce the correct result" $ do
        m1 * m2 `shouldBe` m1TimesM2

    context "when multiplied by tuples" $ do
      let matrix = Matrix4 (V4 1 2 3 4)
                           (V4 2 4 4 2)
                           (V4 8 6 4 1)
                           (V4 0 0 0 1)

      let tuple = Tuple 1 2 3 1

      let matrixTimesTuple = Tuple 18 24 33 1

      it "should produce the correct result" $ do
        matrix *|| tuple `shouldBe` matrixTimesTuple

    context "identity matrix" $ do
      it "should behave like an idendity matrix :)" $ do
        identity4 *|| Tuple 1 2 3 4 `shouldBe` Tuple 1 2 3 4

    context "transpose" $ do
      let matrix = Matrix4 (V4 0 9 3 0)
                           (V4 9 8 0 8)
                           (V4 1 8 5 3)
                           (V4 0 0 5 8)

      let transposed = Matrix4 (V4 0 9 1 0)
                               (V4 9 8 8 0)
                               (V4 3 0 5 5)
                               (V4 0 8 3 8)

      it "should swap the rows and columns" $ do
        transpose matrix `shouldBe` transposed

      it "should transpose the identity matrix into the identity matrix" $ do
        transpose identity4 `shouldBe` identity4


  describe "3x3 Matrices" $ do
    context "when constructed" $ do
      let m = Matrix3 (V3  (-3) 5    0   )
                      (V3  1    (-2) (-7))
                      (V3  0    1    1   )
      
      it "puts the elements in the right places" $ do
        m !? [0,0] `shouldBe` (-3)
        m !? [1,1] `shouldBe` (-2)
        m !? [2,2] `shouldBe` 1

  describe "2x2 Matrices" $ do
      context "when constructed" $ do
        let m = Matrix2 (V2  (-3) 5   )
                        (V2  1    (-2))
        
        it "puts the elements in the right places" $ do
          m !? [0,0] `shouldBe` (-3)
          m !? [0,1] `shouldBe` 5
          m !? [1,0] `shouldBe` 1
          m !? [1,1] `shouldBe` (-2)

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


