module Main ( main ) where

import Tuples
import Canvas
import Colors

data Projectile = Projectile { position :: Tuple, velocity :: Tuple } deriving (Eq, Show)
data Environment = Environment { gravity :: Tuple, wind :: Tuple }


main :: IO()
main = do
  let projectile = Projectile (point 0 1 0) ((normalize (vector 1 1.8 0)) *| 11.25)
  let environment = Environment (vector 0 (-0.1) 0) (vector (-0.01) 0 0)
  let positions = map (toCanvasCoordinates 550 . position) (runSimulation environment projectile)
  let canvas = foldl (\canv (pos, col) -> writePixel canv pos col) (newCanvas 990 550) (zip positions (cycle [Color 1 0 0]))
  putStr (constructPpm canvas)

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
