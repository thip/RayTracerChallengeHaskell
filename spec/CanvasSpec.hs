module CanvasSpec where

import Test.Hspec
import Canvas
import Colors

main :: IO ()
main = hspec $ do
  describe "Creating a canvas" $ do
    let canvas = newCanvas 10 20

    it "has a width of 10" $ do
      width canvas `shouldBe` 10

    it "has a height of 20" $ do
      height canvas `shouldBe` 20

    it "contains only black pixels" $ do
      all (Color 0 0 0 ==) (pixels canvas) `shouldBe` True

  describe "Creating a colored canvas" $ do
    let background = Color 0.2 0.5 0.7
    let canvas = newCanvasWithColor 10 20 background

    it "contains only pixels matching the provided color" $ do
      all (background ==) (pixels canvas) `shouldBe` True

  describe "Writing a pixel to a canvas" $ do
    let canvas = newCanvas 10 20
    let red = Color 1 0 0
    let updatedCanvas = writePixel canvas (2, 3) red

    it "stores the correct color pixel in the correct position of the canvas" $ do
      pixelAt updatedCanvas (2, 3) `shouldBe` red

    context "when the pixel is outside the bounds of the canvas" $ do
      let updatedCanvas2 = writePixel canvas (10, 10) red

      it "doesn't write the pixel" $ do
        pixels updatedCanvas2 `shouldBe` pixels canvas

  describe "Constructing a ppm file from the canvas" $ do
    let canvas = newCanvas 5 3

    let c1 = Color 1.5 0 0
    let c2 = Color 0 0.5 0
    let c3 = Color (-0.5) 0 1

    let updatedCanvas = foldl (\canv (pos, col) -> writePixel canv pos col) canvas [((0,0), c1), ((2,1), c2), ((4,2), c3)]

    let ppm = constructPpm updatedCanvas

    it "constructs the ppm header" $ do
      take 3 (lines ppm) `shouldBe` ["P3", "5 3", "255"]

    it "constructs the ppm data" $ do
      drop 3 (lines ppm) `shouldBe`
        ["255 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
        "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0",
        "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"]

    it "splits lines longer than 70 charaters" $ do
      let ppmWithSplitLines = constructPpm $ newCanvasWithColor 10 2 (Color 1 0.8 0.6)
      drop 3 (lines ppmWithSplitLines) `shouldBe`
        ["255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204",
        "153 255 204 153 255 204 153 255 204 153 255 204 153",
        "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204",
        "153 255 204 153 255 204 153 255 204 153 255 204 153"]

    it "terminates file with a newline character" $ do
      last ppm `shouldBe` '\n'