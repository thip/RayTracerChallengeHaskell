module Canvas (
  newCanvas,
  newCanvasWithColor,
  width,
  height,
  pixels,
  writePixel,
  pixelAt,
  constructPpm
) where

import Data.Array
import Colors

newtype Canvas = Canvas (Array (Int, Int) Color)

newCanvas :: Int -> Int -> Canvas
newCanvas width height = newCanvasWithColor width height (Color 0 0 0)

newCanvasWithColor :: Int -> Int -> Color -> Canvas
newCanvasWithColor width height color = Canvas (array ((0,0),(height-1, width-1)) [((y,x), color) | x <- [0..width-1], y <- [0..height-1]])

width :: Canvas -> Int
width (Canvas pixels) = (\((_,_),(_,width)) -> width + 1) (bounds pixels)

height :: Canvas -> Int
height (Canvas pixels) = (\((_,_),(height,_)) -> height + 1) (bounds pixels)

pixels :: Canvas -> [Color]
pixels (Canvas pixels) = elems pixels

writePixel :: Canvas -> (Int, Int) -> Color -> Canvas
writePixel (Canvas pixels) (x, y) color 
  | x < 0 = (Canvas pixels)
  | y < 0 = (Canvas pixels)
  | x > width (Canvas pixels) - 1 = (Canvas pixels)
  | y > height (Canvas pixels) - 1 = (Canvas pixels)
  | otherwise = Canvas (pixels // [((y,x), color)])

pixelAt :: Canvas -> (Int, Int) -> Color
pixelAt (Canvas pixels) (x, y) = pixels ! (y, x)

constructPpm :: Canvas -> String
constructPpm canvas = unlines $ (constructPpmHeaderLines canvas) ++ (constructPpmDataLines canvas)

constructPpmHeaderLines :: Canvas -> [String]
constructPpmHeaderLines canvas = ["P3", show (width canvas) ++ " " ++ show (height canvas), "255"]

constructPpmDataLines :: Canvas -> [String]
constructPpmDataLines canvas = wrapLines 17 ((pixels canvas) `toRowsWithWidth` (width canvas))

wrapLines :: Int -> [[String]] -> [String]
wrapLines maxLineLength lines = map unwords $ wrapRowsAt maxLineLength $ map words $ map unwords lines

toRowsWithWidth :: [Color] -> Int -> [[String]]
[] `toRowsWithWidth` _ = []
pixels `toRowsWithWidth` width = (splitListAt width (map toColorString (pixels)))

wrapRowsAt :: Int -> [[a]] -> [[a]]
wrapRowsAt n [] =[]
wrapRowsAt n (l:rest) = (splitListAt n l) ++ wrapRowsAt n rest

splitListAt :: Int -> [a] -> [[a]]
splitListAt _ [] = []
splitListAt n list = [take n list ] ++ splitListAt n (drop n list)

toColorString :: Color -> String
toColorString (Color r g b) = unwords $ map toComponentString [r,g,b]

toComponentString :: Double -> String
toComponentString component = show $ clampNumber $ scaleNumber component

scaleNumber :: Double -> Int
scaleNumber number = round (number * 255)

clampNumber :: Int -> Int
clampNumber number
  | number > 255 = 255
  | number < 0 = 0
  | otherwise = number