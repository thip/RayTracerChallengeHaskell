module ProjectileSimulator (
  Projectile (..),
  Environment (..),
  toCanvasCoordinates,
  tick,
  runSimulation,
  isInFlight
) where

import Tuples
import Canvas
import Colors

data Projectile = Projectile { position :: Tuple, velocity :: Tuple } deriving (Eq, Show)
data Environment = Environment { gravity :: Tuple, wind :: Tuple }

toCanvasCoordinates :: Int -> Tuple -> (Int, Int)
toCanvasCoordinates canvasHeight (Tuple x y _ _) = (round x, round ((fromIntegral canvasHeight)-y))

tick :: Environment -> Projectile -> Projectile
tick (Environment gravity wind) (Projectile position velocity) = Projectile (position + velocity) (velocity + gravity + wind)

runSimulation :: Environment -> Projectile -> [Projectile]
runSimulation environment projectile
  | isInFlight projectile = [projectile] ++ runSimulation environment (tick environment projectile)
  | otherwise = [projectile]

isInFlight :: Projectile -> Bool
isInFlight (Projectile (Tuple _ y _ _) _)
  | y > 0 = True
  | otherwise = False
