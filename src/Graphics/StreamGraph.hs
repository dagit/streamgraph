module Graphics.StreamGraph where

import Wumpus.Core.Picture ( vertexPath, zcstroke, frame, zfill,
                             zostroke, frameMulti, fill )
import Wumpus.Core.Geometry ( Point2(P2) )
import Wumpus.Core ( Path, Primitive, Picture )
import Wumpus.Core.OutputSVG ( writeSVG_latin1 )
import Wumpus.Core.Colour ( red, green, blue )

{- it looks like wumpus uses the postcript coordinate system:
(0, 100) --------------------- (100, 100)
 |
 |
 |
 |
 |
(0, 0)                         (100, 0)
-}


testBand :: Path Double
testBand = vertexPath [P2 0 0, P2 0 10, P2 10 10, P2 20 15, P2 30 5, P2 30 0]

testPrimitive :: Primitive Double
testPrimitive = zfill testBand

testPicture :: Picture Double
testPicture = frame testPrimitive

-- | Takes the start (x0,y0), the end (x1,y1) and an x.  Gives the y
-- at x via linear interpolation.  Division by zero (when x1 = x0) is
-- the callers responsibility.
lerp :: (Double, Double) -> (Double, Double) -> Double -> Double
lerp (x0,y0) (x1,y1) x = y0 + (x - x0) * (y1 - y0) / (x1 - x0)

-- | Interpolate a function given a set of (x,y) points on the curve.
-- Note1: Assumes the set of points are sorted by x, from smallest to
-- largest.
-- Note2: This will fail with a pattern match failure, if x is
-- requested outside the set of points provided.
-- TODO: Make this return Nothing outside of the set of points provided.
lerpFromPoints :: [(Double, Double)] -> Double -> Double
lerpFromPoints points x = lerp (x0,y0) (x1,y1) x
  where
  ((x0,y0),(x1,y1)) = between points x

-- | Looks for an x between elements in a sorted list of (x,y) pairs.
-- Assumes the list is sorted by x, from 0 to inf.
-- Note: This is partial, any way to rewrite it?
between :: [(Double, Double)] -> Double -> ((Double,Double), (Double, Double))
between ((x0,y0):(x1,y1):xs) x 
  | x0 <= x && x <= x1 = ((x0,y0),(x1,y1))
  | otherwise          = between ((x1,y1):xs) x
between xs x = error $ "x = " ++ show x ++ ", not found between any elements: " ++ show xs

-- Example Height functions
testSeries1 :: Double -> Double
testSeries1 = lerpFromPoints points
  where points = [(0, 10), (10, 10), (20, 15), (30, 5)] ++ [(x,5*sin (x^2) + 10) | x <- [35, 40 .. 1000] ]

testSeries2 :: Double -> Double
testSeries2 = lerpFromPoints points
  where points = [ (x,5*abs(sin x/cos x) + 30) | x <- [0,5..1000] ]

testSeries3 :: Double -> Double
testSeries3 = lerpFromPoints points
  where points = [ (x, x/2 - x/3 + x/4 - x/5) | x <- [1,5..1000] ]

fs :: [Double -> Double]
fs = [ testSeries1, testSeries2, testSeries3 ]

g0 :: Double -> Double
g0 x = - 0.5 * sum (fis x)
  where fis x = zipWith ($) fs (repeat x)

gi :: Int -> Double -> Double
gi i x = g0 x + sum (fis x)
  where fis x = take i $ zipWith ($) fs (repeat x)

-- | Add two functions at a point
(.+) :: Num a => (a -> a) -> (a -> a) -> (a -> a)
(f .+ g) x = (f x) + (g x)

gs :: [Double -> Double]
gs = map (g0 .+) (scanl (.+) (const 0) fs)

band1 :: Path Double
band1 = vertexPath ps
  where
  ps = [P2 x ((gs !! 0) x) | x <- [1,5 .. 800] ] ++
       reverse [P2 x ((gs !! 1) x) | x <- [1,5 .. 800] ]

band2 :: Path Double
band2 = vertexPath ps
  where
  ps = [P2 x ((gs !! 1) x) | x <- [1,5 .. 800] ] ++
       reverse [P2 x ((gs !! 2) x) | x <- [1,5 .. 800] ]

band3 :: Path Double
band3 = vertexPath ps
  where
  ps = [P2 x ((gs !! 2) x) | x <- [1,5 .. 800] ] ++
       reverse [P2 x ((gs !! 3) x) | x <- [1,5 .. 800] ]

f1 = frameMulti $ [fill red band1, fill green band2, fill blue band3]