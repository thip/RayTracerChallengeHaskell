module ProjectileSimulatorSpec where

import Test.Hspec
import Tuples
import ProjectileSimulator

main :: IO ()
main = hspec $ do
  describe "tick" $ do 
    context "in an environment with no wind or gravity and a moving projectile" $ do
      let environment = Environment (vector 0 0 0) (vector 0 0 0)
      let projectile = Projectile (point 1 2 3) (vector 1 2 3)

      it "just adds the velocity of the projectile to its position" $ do
        tick environment projectile `shouldBe` Projectile (point 2 4 6) (vector 1 2 3)

    context "in an environment with gravity and a stationary projectile" $ do
      let environment = Environment (vector 1 2 3) (vector 0 0 0)
      let projectile = Projectile (point 1 2 3) (vector 0 0 0)

      it "accelerates the projectile in the direction of gravity" $ do
        tick environment projectile `shouldBe` Projectile (point 1 2 3) (vector 1 2 3)

    context "in an environment with wind and a stationary projectile" $ do
      let environment = Environment (vector 0 0 0) (vector 3 4 5)
      let projectile = Projectile (point 1 2 3) (vector 0 0 0)

      it "accelerates the projectile in the direction of the wind" $ do
        tick environment projectile `shouldBe` Projectile (point 1 2 3) (vector 3 4 5)

  describe "isInFlight" $ do
    context "when the projectile is above the ground" $ do
      it "returns false" $ do
        isInFlight (Projectile (point 1 2 3) (vector 0 0 0)) `shouldBe` True

    context "when the projectile is on the ground" $ do
          it "returns true" $ do
            isInFlight (Projectile (point 0 0 0) (vector 0 0 0)) `shouldBe` False

    context "when the projectile is below the ground" $ do
          it "returns true" $ do
            isInFlight (Projectile (point 0 (-1) 0) (vector 0 0 0)) `shouldBe` False
