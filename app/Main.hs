module Main where

import Data
import Lib
import System.IO
import System.Random

main :: IO ()
main = do
  gen <- newStdGen
  let filledInGrid = fillInBlanks gen grid 
      game = makeGame filledInGrid languages
  hSetBuffering stdout NoBuffering
  playTurn game

playTurn game = do
  putStrLn . formatGame $ game
  putStr "Please enter a word> "
  -- the arrow allows us to get information from the input
  word <- getLine
  let newGame = playGame game word
  if completed newGame then
    putStrLn "Congratulations!"
  else
    playTurn newGame
