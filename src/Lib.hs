module Lib
  ( formatGrid,
    outputGrid,
    findWord,
    findWordInLine,
    findWords,
    findWordInCellLinePrefix,
    skew,
    zipOverGrid,
    zipOverGridWith,
    gridWithCoords,
    cell2char,
    Cell (Cell, Indent),
    Game (gameGrid, gameWords),
    makeGame,
    totalWords,
    score,
    completed,
    playGame,
    formatGame,
    makeRandomGrid,
    fillInBlanks
  )
where

import Data.List (isInfixOf, transpose)
import qualified Data.Map as M
import Data.Maybe (catMaybes, listToMaybe)
import System.Random 

data Game = Game {
              gameGrid :: Grid Cell,
              gameWords :: M.Map String (Maybe [Cell])
            }
            deriving Show 

data Cell
  = Cell (Integer, Integer) Char
  | Indent
  deriving (Eq, Ord, Show)

type Grid a = [[a]]

makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
  let gwc = gridWithCoords grid
      tuplify word = (word, Nothing)
      list = map tuplify words
      dict = M.fromList list
  in Game gwc dict 

totalWords :: Game -> Int 
totalWords game = length . M.keys $ gameWords game 

score :: Game -> Int
score game = length . catMaybes . M.elems $ gameWords game

completed :: Game -> Bool
completed game = score game == totalWords game

playGame :: Game -> String -> Game 
-- guard function to prevent score increase from partially correct input
playGame game word | not $ M.member word (gameWords game) = game 
playGame game word = 
  let grid = gameGrid game
      foundWord = findWord grid word
  in case foundWord of
      Nothing -> game
      Just cs -> 
        let dict = gameWords game
            newDict = M.insert word foundWord dict
        in game { gameWords = newDict }


formatGame :: Game -> String
formatGame game =
  let grid = gameGrid game
  in formatGrid grid
     ++ "\n\n"
     ++ (show $ score game)
     ++ "/"
     ++ (show $ totalWords game)

makeRandomGrid gen =
  let (gen1, gen2) = split gen 
     row = randomRs ('A', 'Z') gen1
  in row : makeRandomGrid gen2

fillInBlanks gen grid =
  let r = makeRandomGrid gen
      fill '_' r = r
      fill c _ = c 
  in zipOverGridWith fill grid r

zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

coordsGrid :: Grid (Integer, Integer)
coordsGrid =
  let rows = map repeat [0 ..]
      cols = repeat [0 ..]
   in zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords = zipOverGridWith Cell coordsGrid

outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cell2char

cell2char :: Cell -> Char
-- pattern matching
cell2char (Cell _ c) = c
cell2char Indent = '?'

getLines :: Grid Cell -> [[Cell]]
getLines grid =
  let horizontal = grid
      vertical = transpose grid
      diagonal1 = diagonalize grid
      diagonal2 = diagonalize (map reverse grid)
      lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
   in lines ++ (map reverse lines)

diagonalize :: Grid Cell -> Grid Cell
-- composed function of transpose and skew
diagonalize = transpose . skew

skew :: Grid Cell -> Grid Cell
-- base case
skew [] = []
skew (l : ls) = l : skew (map indent ls)
  where
    indent line = Indent : line

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
  let lines = getLines grid
      foundWords = map (findWordInLine word) lines
   in listToMaybe (catMaybes foundWords)

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words =
  let foundWords = map (findWord grid) words
   in catMaybes foundWords

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line =
  let found = findWordInCellLinePrefix [] word line
   in case found of
        Nothing -> findWordInLine word (tail line)
        cs@(Just _) -> cs

-- recursive function
findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
-- good case
findWordInCellLinePrefix acc (x : xs) (c : cs)
  | x == cell2char c =
    findWordInCellLinePrefix (c : acc) xs cs
-- base cases
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing