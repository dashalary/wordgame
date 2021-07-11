module Main where

import Data
import Lib

main :: IO ()
main = do
  let game = makeGame grid languages
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
