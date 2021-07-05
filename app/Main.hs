module Main where

import Data
import Lib

main :: IO ()
main =
  let gwc = gridWithCoords grid
   in outputGrid gwc
