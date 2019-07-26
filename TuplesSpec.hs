module TuplesSpec where

import Test.Hspec
import Tuples

main :: IO ()
main = hspec $ do
  describe "A tuple" $ do
    context "created with x=4.2, y=-4.2, z=3.1, w=1.0" $ do
      let t = Tuple (4.3) (-4.2) (3.1) (1.0)
      
      it "has an x component of 4.3" $ do
        x t `shouldBe` 4.3

      it "has a y component of -4.2" $ do
        y t `shouldBe` (-4.2)

      it "has a z component of 3.1" $ do
        z t `shouldBe` 3.1
      
      it "has a w component of 1.0" $ do
        w t `shouldBe` 1.0
      
      it "is a point" $ do
        isPoint t `shouldBe` True

      it "is not a vector" $ do
        isVector t `shouldBe` False
    
    context "created with x=5.1, y=4.7, z=-3.6, w=0.0" $ do
      let t = Tuple (5.1) (4.7) (-3.6) (0.0)
      
      it "has an x component of 5.1" $ do
        x t `shouldBe` 5.1

      it "has a y component of 4.7" $ do
        y t `shouldBe` 4.7

      it "has a z component of -3.6" $ do
        z t `shouldBe` (-3.6)
      
      it "has a w component of 0.0" $ do
        w t `shouldBe` 0.0
      
      it "is not a point" $ do
        isPoint t `shouldBe` False

      it "is a vector" $ do
        isVector t `shouldBe` True
  
  describe "point" $ do
    it "creates a tuple with w=1" $ do
      point 4 (-4) 3 `shouldBe` Tuple 4 (-4) 3 1.0

  describe "vector" $ do
    it "creates a tuple with w=0" $ do
      vector 4 (-4) 3 `shouldBe` Tuple 4 (-4) 3 0

  describe "adding two tuples" $ do
    let t1 = Tuple 3 (-2) 5 1
    let t2 = Tuple (-2) 3 1 0

    it "adds each component of the tuple" $ do
      t1 + t2 `shouldBe` Tuple 1 1 6 1

  describe "subtracting two points" $ do
    let p1 = point 3 2 1
    let p2 = point 5 6 7

    it "gives the vector between the points" $ do
      p1 - p2 `shouldBe` vector (-2) (-4) (-6)

  describe "subtracting a vector from a point" $ do
    let p = point 3 2 1
    let v = vector 5 6 7

    it "translates the point backwards along the vector" $ do
      p - v `shouldBe` point (-2) (-4) (-6)

  describe "subtracting vectors" $ do
    it "gives a vector representing the change in direction between the two vectors" $ do
      let v1 = vector 3 2 1
      let v2 = vector 5 6 7

      v1 - v2 `shouldBe` vector (-2) (-4) (-6)

    it "negates the second vector when the first vector is 0" $ do
      let zero = vector 0 0 0
      let v = vector 1 (-2) 3

      zero - v `shouldBe` vector (-1) 2 (-3)

  describe "negating a tuple" $ do
    let t = Tuple 1 (-2) 3 (-4)

    it "changes all the signs" $ do
      (-t) `shouldBe` Tuple (-1) 2 (-3) 4

  describe "Multiplying a tuple" $ do
    let t = Tuple 1 (-2) 3 (-4)

    context "by a scalar" $ do
      it "Multiplies each component of the tuple by the scalar" $ do
        t *| 3.5 `shouldBe` Tuple 3.5 (-7) 10.5 (-14)

    context "by a fraction" $ do
      it "Multiplies each component of the tuple by the fraction" $ do
        0.5 |* t `shouldBe` Tuple 0.5 (-1) 1.5 (-2)

  describe "Dividing a tuple by a scalar" $ do
    let t = Tuple 1 (-2) 3 (-4)

    it "divides each component of the tuple by the scalar" $ do
      t /| 2 `shouldBe` Tuple 0.5 (-1) 1.5 (-2)

  describe "Computing the magnitude" $ do
    context "of vector 0 1 0" $ do
      let v = vector 0 1 0

      it "results in 1" $ do
        magnitude v `shouldBe` 1

    context "of vector 0 0 1" $ do
      let v = vector 0 0 1

      it "results in 1" $ do
        magnitude v `shouldBe` 1

    context "of vector 1 2 3" $ do
      let v = vector 1 2 3

      it "results in 1" $ do
        magnitude v `shouldBe` sqrt 14

    context "of vector -1 -2 -3" $ do
      let v = vector (-1) (-2) (-3)

      it "results in 1" $ do
        magnitude v `shouldBe` sqrt 14

  describe "Normalizing vectors" $ do
    context "Given vector 4 0 0" $ do
      let v = vector 4 0 0
      it "normalizes to 1 0 0" $ do
        normalize v `shouldBe` vector 1 0 0

    context "Given vector 1 2 3" $ do
      let v = vector 1 2 3
      it "normalizes to 1/√14 2/√14 3/√14" $ do
        normalize v `shouldBe` vector (1 / sqrt 14) (2 / sqrt 14) (3 / sqrt 14)

    context "The magnitude of a normalized vector" $ do
      let normalized = normalize $ vector 1 2 3

      it "should be approximately 1" $ do
        magnitude normalized `shouldSatisfy` (isApproximately 1)

    describe "Dot product" $ do
      context "of vector 1 2 3, and vector 2 3 4" $ do
        let v1 = vector 1 2 3
        let v2 = vector 2 3 4

        it "is 20" $ do
          v1 `dot` v2 `shouldBe` 20

    describe "Cross product" $ do
      context "of vector 1 2 3, and vector 2 3 4" $ do
        let v1 = vector 1 2 3
        let v2 = vector 2 3 4

        it "is vector -1 2 -1" $ do
          v1 `cross` v2 `shouldBe` vector (-1) 2 (-1)

      context "of vector 2 3 4, and vector 1 2 3" $ do
        let v1 = vector 2 3 4
        let v2 = vector 1 2 3

        it "is vetror 1 -2 1" $ do
          v1 `cross` v2 `shouldBe` vector 1 (-2) 1


isApproximately :: Float -> Float -> Bool
isApproximately a b =  0.000001 > abs(a - b)