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


