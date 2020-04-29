module Matrices4Spec (spec) where

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

      let m2 = Matrix4 (V4 (-2) 1  2   3)
                       (V4   3  2  1 (-1))
                       (V4   4  3  6   5)
                       (V4   1  2  7   8)

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

    context "sub matrix" $ do
      let m4x4 = Matrix4 (V4 (-6)  1   1   6)
                         (V4 (-8)  5   8   6)
                         (V4 (-1)  0   8   2)
                         (V4 (-7)  1 (-1)  1)

      let sub = Matrix3 (V3 (-6)   1   6)
                        (V3 (-8)   8   6)
                        (V3 (-7) (-1)  1)

      it "is a 3x3 matrix where the provided row and column of the original matrix have been removed" $ do
        subMatrix4 m4x4 2 1 `shouldBe` sub

    context "cofactor" $ do
      let m = Matrix4 (V4 (-2) (-8)   3   5)
                      (V4 (-3)   1    7   3)
                      (V4   1    2  (-9)  6)
                      (V4 (-6)   7    7 (-9))

      it "is correct at 0 0" $ do
        cofactor4 m 0 0 `shouldBe` 690

      it "is correct at 0 1" $ do
        cofactor4 m 0 1 `shouldBe` 447

      it "is correct at 0 2" $ do
        cofactor4 m 0 2 `shouldBe` 210

      it "is correct at 0 3" $ do
        cofactor4 m 0 3 `shouldBe` 51

    context "determinant" $ do
      let m = Matrix4 (V4 (-2) (-8)  3   5)
                      (V4 (-3)   1   7   3)
                      (V4   1    2 (-9)  6)
                      (V4 (-6)   7   7 (-9))

      it "is calculated correctly" $ do
        determinant4 m `shouldBe` (-4071)

    context "inversion" $ do
      it "is invertable when the determinant of the matrix is not 0" $ do
        let m = Matrix4 (V4 6   4   4   4)
                        (V4 5   5   7   6)
                        (V4 4 (-9)  3 (-7))
                        (V4 9   1   7 (-6))

        determinant4 m `shouldBe` (-2120)
        invertable4 m `shouldBe` True

      it "is not invertable when the determinant of the matrix is 0" $ do
        let m = Matrix4 (V4 (-4)   2  (-2) (-3))
                        (V4   9    6    2    6)
                        (V4   0  (-5)   1  (-5))
                        (V4   0    0    0    0)

        determinant4 m `shouldBe` 0
        invertable4 m `shouldBe` False

      it "calculates the inverse correctly" $ do
        let m = Matrix4 (V4 (-5)   2    6  (-8))
                        (V4   1  (-5)   1    8)
                        (V4   7    7  (-6) (-7))
                        (V4   1  (-3)   7    4)

        let inverted = Matrix4 (V4 0.21804511278195488 0.45112781954887216 0.24060150375939848 (-4.5112781954887216e-2))
                               (V4 (-0.8082706766917294) (-1.4567669172932332) (-0.44360902255639095) 0.5206766917293233)
                               (V4 (-7.894736842105263e-2) (-0.2236842105263158) (-5.263157894736842e-2) 0.19736842105263158)
                               (V4 (-0.5225563909774437) (-0.8139097744360902) (-0.3007518796992481) 0.30639097744360905)

        inverse4 m `shouldBe` inverted