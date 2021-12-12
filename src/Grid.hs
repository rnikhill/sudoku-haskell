module Grid where

import Control.Monad
import Data.Array
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Text.Read

data Square = Fixed Int | Possible (Set.Set Int)
data Board = Board Int (Array (Int, Int) Square)

isEmpty :: Square -> Bool
isEmpty (Possible _) = True
isEmpty _ = False

toString :: Square -> String
toString (Possible _) = "*"
toString (Fixed n) = show n

getNumbers :: [Square] -> Set.Set Int
getNumbers squares = Set.fromList [x | Fixed x <- squares]

getRow :: Board -> Int -> [Square]
getRow (Board rank arr) i = [arr ! (i, j) | j <- [1 .. rank ^ 2]]

getCol :: Board -> Int -> [Square]
getCol (Board rank arr) j = [arr ! (i, j) | i <- [1 .. rank ^ 2]]

getSector :: Board -> (Int, Int) -> [Square]
getSector (Board rank arr) (i, j) =
  let is = [(i - 1) * rank + 1 .. i * rank]
      js = [(j - 1) * rank + 1 .. j * rank]
   in [arr ! (i, j) | i <- is, j <- js]

getRowNeighbors :: Board -> (Int, Int) -> [Square]
getRowNeighbors board (i, _) = getRow board i

getColNeighbors :: Board -> (Int, Int) -> [Square]
getColNeighbors board (_, j) = getCol board j

getSectorNeighbors :: Board -> (Int, Int) -> [Square]
getSectorNeighbors board@(Board rank arr) (i, j) =
  let i' = (i + rank - 1) `div` rank
      j' = (j + rank - 1) `div` rank
   in getSector board (i', j')

getMissingNums :: Int -> [Square] -> Set.Set Int
getMissingNums max squares =
  let nums = [x | Fixed x <- squares] & Set.fromList
   in [1 .. max] & filter (\x -> Set.notMember x nums) & Set.fromList

hasDuplicates :: [Square] -> Bool
hasDuplicates xs = (Set.size $ getNumbers xs) == length xs

getPotentialValues :: (Int, Int) -> Board -> Set.Set Int
getPotentialValues (i, j) board@(Board rank arr) =
  let missingR = getMissingNums (rank ^ 2) $ getRowNeighbors board (i, j)
      missingC = getMissingNums (rank ^ 2) $ getColNeighbors board (i, j)
      missingS = getMissingNums (rank ^ 2) $ getSectorNeighbors board (i, j)
   in missingR `Set.intersection` missingC `Set.intersection` missingS

isSolved :: Board -> Bool
isSolved board@(Board rank arr) =
  let noRowDups = all (\i -> hasDuplicates $ getRow board i) [1 .. rank ^ 2]
      noColDups = all (\j -> hasDuplicates $ getCol board j) [1 .. rank ^ 2]
      noSecDups = all (\(i, j) -> hasDuplicates $ getSector board (i, j)) [(i, j) | i <- [1 .. rank], j <- [1 .. rank]]
   in (length (getEmptySquares board) == 0) && noRowDups && noColDups && noSecDups

solveSudokuImpl :: [(Int, Int)] -> Board -> Maybe Board
solveSudokuImpl empties board@(Board rank arr) = case empties of
        [] | isSolved board -> Just board
        [] -> Nothing
        (i, j) : xs ->
          let
            potentials = getPotentialValues (i,j) board
            nextStates = [Board rank $ arr // [((i, j), Fixed p)] | p <- Set.toList potentials]
            solutions = map (solveSudokuImpl xs) nextStates
            sol = find isJust solutions
          in
            if isJust sol then fromJust sol else Nothing

solveSudoku :: Board -> Maybe Board
solveSudoku board = solveSudokuImpl (getEmptySquares board) board


getEmptySquares :: Board -> [(Int, Int)]
getEmptySquares (Board rank arr) =
  [(i, j) | i <- [1 .. rank ^ 2], j <- [1 .. rank ^ 2], isEmpty (arr ! (i, j))]

printBoard :: Board -> IO ()
printBoard board@(Board rank arr) = forM_ [1 .. rank ^ 2] $ \i -> do
  forM_ [1 .. rank ^ 2] $ \j -> do
    let square = arr ! (i, j)
    putStr $ toString square
    putStr " "
    if j `mod` rank == 0 && j /= rank ^ 2 then putStr "|" else return ()
  putStr "\n"
  if i `mod` rank == 0 && i /= rank ^ 2
    then do
      forM_ [1 .. rank ^ 2] $ \j -> do
        putStr "--"
        if j `mod` rank == 0 && j /= rank ^ 2 then putStr "+" else return ()
      putStr "\n"
    else return ()

getRank :: IO (Int)
getRank = do
  putStrLn "Please enter rank of board"
  line <- getLine
  return $ read line

readRow :: Int -> [((Int, Int), Square)] -> Int -> IO ([((Int, Int), Square)])
readRow rank soFar i = do
  line <- getLine
  let filtered = filter (/= '|') line
      nums = words filtered
      maybes = map readMaybe nums
      maybe2square = \x -> case x of
        Nothing -> Possible $ Set.fromList [1..rank^2]
        Just n -> Fixed n
      squares = map maybe2square maybes
      indices = [(i, j) | j <- [1 .. rank ^ 2]]
  if i `mod` rank == 0 && i /= rank ^ 2 then getLine else return ""
  return $ soFar ++ (indices `zip` squares)

readBoard :: IO (Board)
readBoard = do
  rank <- getRank
  indices <- foldM (readRow rank) [] [1 .. rank ^ 2]
  return $ Board rank $ array ((1, 1), (rank ^ 2, rank ^ 2)) indices
