module Lib
  ( formatGrid,
    outputGrid,
    findWord,
    findWordInLine,
    findWords,
    skew,
  )
where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

data Cell = Cell (Integer, Integer) Char deriving (Eq, Ord, Show)

type Grid a = [[a]]

zipOverGrid = zipWith zip

zipOverGridWith = zipWith . zipWith

coordsGrid =
  let rows = map repeat [0 ..]
      cols = repeat [0 ..]
   in zipOverGrid rows cols

gridWithCoords = zipOverGridWith Cell coordsGrid

outputGrid :: Grid Char -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid Char -> String
formatGrid = unlines

getLines :: Grid Char -> [String]
getLines grid =
  let horizontal = grid
      vertical = transpose grid
      diagonal1 = diagonalize grid
      diagonal2 = diagonalize (map reverse grid)
      lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
   in lines ++ (map reverse lines)

diagonalize :: Grid -> Grid
-- composed function of transpose and skew
diagonalize = transpose . skew

skew :: Grid -> Grid
-- base case
skew [] = []
skew (l : ls) = l : skew (map indent ls)
  where
    indent line = '_' : line

findWord :: Grid -> String -> Maybe String
findWord grid word =
  let lines = getLines grid
      found = or $ map (findWordInLine word) lines
   in if found then Just word else Nothing

findWords :: Grid -> [String] -> [String]
findWords grid words =
  let foundWords = map (findWord grid) words
   in catMaybes foundWords

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf