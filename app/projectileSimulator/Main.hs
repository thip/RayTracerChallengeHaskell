module Main ( main ) where

import Tuples
import Colors
import Canvas
import ProjectileSimulator

main :: IO()
main = do
  let projectile = Projectile (point 0 1 0) ((normalize (vector 1 1.8 0)) *| 11.25)
  let environment = Environment (vector 0 (-0.1) 0) (vector (-0.01) 0 0)
  let positions = map (toCanvasCoordinates 550 . position) (runSimulation environment projectile)
  let canvas = foldl (\canv (pos, col) -> writePixel canv pos col) (newCanvas 990 550) (zip positions (cycle [Color 1 0 0]))
  putStr (constructPpm canvas)