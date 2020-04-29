module TransformationsSpec (spec) where

import Test.Hspec
import Transformations
import Matrices
import Tuples

spec :: Spec
spec = do
  describe "Translation" $ do
    let transform = translation 5 (-3) 2
    let start = point (-3) 4 5
    let end = point 2 1 7

    it "translates the point correctly" $ do
      (transform *|| start) `shouldBe` end

    it "translates the point the other direction when inverted" $ do
      (inverse4 transform *|| end) `shouldBe` start

    it "does not affect vectors" $ do
      let vec = vector (-3) 44 5
      (transform *|| vec) `shouldBe` vec