module ColorsSpec (spec) where

import Test.Hspec
import SpecHelpers
import Tuples
import Colors

spec :: Spec
spec = do
  describe "A Color" $ do
    context "created with red=4.2, green=-4.2, blue=3.1" $ do
      let c = Color (-0.5) (0.4) (1.7)
      
      it "has an red component of -0.5" $ do
        red c `shouldBe` (-0.5)

      it "has a green component of 0.4" $ do
        green c `shouldBe` 0.4

      it "has a blue component of 1.7" $ do
        blue c `shouldBe` 1.7

  describe "Adding Colors" $ do
    let c1 = Color 0.9 0.6 0.75
    let c2 = Color 0.7 0.1 0.25

    it "adds each component of the Colors" $ do
      c1 + c2 `shouldBe` Color 1.6 0.7 1.0

  describe "Subtracting Colors" $ do
    let c1 = Color 0.9 0.6 0.75
    let c2 = Color 0.7 0.1 0.25
    let result = c1 - c2

    it "subtracts each component of the Colors" $ do
      c1 - c2 `shouldSatisfy` isAboutTheSameAs (Color 0.2 0.5 0.5)

  describe "Multiplying a Color by a Scalar" $ do
    let c = Color 0.2 0.3 0.4

    it "multiplies each component of the Color by the scalar" $ do
      c *| 2 `shouldBe` Color 0.4 0.6 0.8

    it "multiplies each component of the Color by the scalar" $ do
      2 |* c `shouldBe` Color 0.4 0.6 0.8


  describe "Multiplying Colors" $ do
    let c1 = Color 1 0.2 0.4
    let c2 = Color 0.9 1 0.1

    it "multiplies the red, green, and blue components" $ do
      c1 * c2 `shouldSatisfy` isAboutTheSameAs (Color 0.9 0.2 0.04)

isAboutTheSameAs :: Color -> Color -> Bool
isAboutTheSameAs a b =
  red a `isApproximately` red b &&
  green a `isApproximately` green b &&
  blue a `isApproximately` blue b
