module Grid where

import Control.Monad
import Data.Array
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Text.Read

data Board = Board Int (Array (Int, Int) (Maybe Int))

mkBoard :: Int -> Board
mkBoard rank = Board rank arr
  where
    arr = array ((1, 1), (rank ^ 2, rank ^ 2)) [((i, j), Nothing) | i <- [1 .. rank ^ 2], j <- [1 .. rank ^ 2]]

getRow :: Board -> Int -> [Maybe Int]
getRow (Board rank arr) i = [arr ! (i, j) | j <- [1 .. rank ^ 2]]

getCol :: Board -> Int -> [Maybe Int]
getCol (Board rank arr) j = [arr ! (i, j) | i <- [1 .. rank ^ 2]]

getSector :: Board -> (Int, Int) -> [Maybe Int]
getSector (Board rank arr) (i, j) =
  let is = [(i - 1) * rank + 1 .. i * rank]
      js = [(j - 1) * rank + 1 .. j * rank]
   in [arr ! (i, j) | i <- is, j <- js]

getRowNeighbors :: Board -> (Int, Int) -> [Maybe Int]
getRowNeighbors board (i, _) = getRow board i

getColNeighbors :: Board -> (Int, Int) -> [Maybe Int]
getColNeighbors board (_, j) = getCol board j

getSectorNeighbors :: Board -> (Int, Int) -> [Maybe Int]
getSectorNeighbors board@(Board rank arr) (i, j) =
  let i' = (i + rank - 1) `div` rank
      j' = (j + rank - 1) `div` rank
   in getSector board (i', j')

getMissingNums :: Int -> [Maybe Int] -> Set.Set Int
getMissingNums max maybes =
  let nums = maybes & (filter isJust) & (map fromJust) & Set.fromList
   in [1 .. max] & filter (\x -> Set.notMember x nums) & Set.fromList

hasDuplicates :: [Maybe Int] -> Bool
hasDuplicates xs = (Set.size $ Set.fromList xs) == length xs

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
            nextStates = [Board rank $ arr // [((i, j), Just p)] | p <- Set.toList potentials]
            solutions = map (solveSudokuImpl xs) nextStates
            sol = find isJust solutions
          in
            if isJust sol then fromJust sol else Nothing

solveSudoku :: Board -> Maybe Board
solveSudoku board = solveSudokuImpl (getEmptySquares board) board


getEmptySquares :: Board -> [(Int, Int)]
getEmptySquares (Board rank arr) =
  [(i, j) | i <- [1 .. rank ^ 2], j <- [1 .. rank ^ 2], isNothing (arr ! (i, j))]

printBoard :: Board -> IO ()
printBoard board@(Board rank arr) = forM_ [1 .. rank ^ 2] $ \i -> do
  forM_ [1 .. rank ^ 2] $ \j -> do
    let tile = arr ! (i, j)
    putStr $ if isJust tile then show (fromJust tile) else "*"
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

readRow :: Int -> [((Int, Int), Maybe Int)] -> Int -> IO ([((Int, Int), Maybe Int)])
readRow rank soFar i = do
  line <- getLine
  let filtered = filter (/= '|') line
      nums = words filtered
      maybes = map readMaybe nums
      indices = [(i, j) | j <- [1 .. rank ^ 2]]
  if i `mod` rank == 0 && i /= rank ^ 2 then getLine else return ""
  return $ soFar ++ indices `zip` maybes

readBoard :: IO (Board)
readBoard = do
  rank <- getRank
  indices <- foldM (readRow rank) [] [1 .. rank ^ 2]
  return $ Board rank $ array ((1, 1), (rank ^ 2, rank ^ 2)) indices
