module Main where

import Graphics.StreamGraph
import Wumpus.Core.OutputSVG

main :: IO ()
main = writeSVG_latin1 "example.svg" f1