{-# LANGUAGE LambdaCase #-}

module Grid where

import Control.Monad
import Data.Array
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Text.Read

data Square = Fixed Int | Possible (Set.Set Int)

type NeighborMap = Map.Map (Int, Int) [(Int, Int)]

data Board = Board Int (Array (Int, Int) Square)

makeNeighborMap :: Int -> NeighborMap
makeNeighborMap rank =
  let ixs = [(i, j) | i <- [1 .. rank ^ 2], j <- [1 .. rank ^ 2]]
      vals = map (getAllNeighborIndices rank) ixs
   in Map.fromList $ ixs `zip` vals

isEmpty :: Square -> Bool
isEmpty (Possible _) = True
isEmpty _ = False

getPossibilities :: Square -> [Int]
getPossibilities (Fixed _) = []
getPossibilities (Possible xs) = Set.toList xs

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

getRowNeighborIndices :: Int -> (Int, Int) -> Set.Set (Int, Int)
getRowNeighborIndices rank (i, _) = Set.fromList [(i, j) | j <- [1 .. rank ^ 2]]

getColNeighborIndices :: Int -> (Int, Int) -> Set.Set (Int, Int)
getColNeighborIndices rank (_, j) = Set.fromList [(i, j) | i <- [1 .. rank ^ 2]]

getSectorNeighborIndices :: Int -> (Int, Int) -> Set.Set (Int, Int)
getSectorNeighborIndices rank (i, j) =
  Set.fromList $
    let i' = (i + rank - 1) `div` rank
        j' = (j + rank - 1) `div` rank
        is = [(i' - 1) * rank + 1 .. i' * rank]
        js = [(j' - 1) * rank + 1 .. j' * rank]
     in [(i, j) | i <- is, j <- js]

getAllNeighborIndices :: Int -> (Int, Int) -> [(Int, Int)]
getAllNeighborIndices rank ix = Set.toList $ (getRowNeighborIndices rank ix) `Set.union` (getColNeighborIndices rank ix) `Set.union` (getSectorNeighborIndices rank ix)

updateAndPrune :: Board -> (Int, Int) -> Int -> NeighborMap -> Board
updateAndPrune board@(Board rank arr) ix@(i, j) val neighborMap =
  let allNeighborIndices = Map.findWithDefault [] ix neighborMap
      allNeighborVals = map (arr !) allNeighborIndices
      neighbors = allNeighborIndices `zip` allNeighborVals
      updates = (ix, Fixed val) : [(ix', Possible (Set.delete val s)) | (ix', Possible s) <- neighbors, ix /= ix']
      arr' = arr // updates
   in Board rank arr'

getMissingNums :: Int -> [Square] -> Set.Set Int
getMissingNums max squares =
  let nums = [x | Fixed x <- squares] & Set.fromList
   in [1 .. max] & filter (`Set.notMember` nums) & Set.fromList

hasNoDuplicates :: [Square] -> Bool
hasNoDuplicates xs = Set.size (getNumbers xs) == length xs

getPotentialValues :: Board -> (Int, Int) -> Set.Set Int
getPotentialValues board@(Board rank arr) (i, j) =
  let missingR = getMissingNums (rank ^ 2) $ getRowNeighbors board (i, j)
      missingC = getMissingNums (rank ^ 2) $ getColNeighbors board (i, j)
      missingS = getMissingNums (rank ^ 2) $ getSectorNeighbors board (i, j)
   in missingR `Set.intersection` missingC `Set.intersection` missingS

isSolved :: Board -> Bool
isSolved board@(Board rank arr) =
  let noRowDups = all (hasNoDuplicates . getRow board) [1 .. rank ^ 2]
      noColDups = all (hasNoDuplicates . getCol board) [1 .. rank ^ 2]
      noSecDups = all (hasNoDuplicates . getSector board) [(i, j) | i <- [1 .. rank], j <- [1 .. rank]]
   in null (getEmptySquares board) && noRowDups && noColDups && noSecDups

solveSudokuImpl :: [(Int, Int)] -> NeighborMap -> Board -> Maybe Board
solveSudokuImpl empties neighborMap board@(Board rank arr) = case empties of
  [] | isSolved board -> Just board
  [] -> Nothing
  ix@(i, j) : xs ->
    let square = arr ! ix
        potentials = getPossibilities square
        nextStates = [updateAndPrune board ix p neighborMap | p <- potentials]
        posLens = xs `zip` map (length . getPossibilities . (arr !)) xs
        sorted = map fst $ sortOn (snd) posLens
        solutions = map (solveSudokuImpl sorted neighborMap) nextStates
        sol = find isJust solutions
     in fromMaybe Nothing sol

solveSudoku :: Board -> Maybe Board
solveSudoku board@(Board rank _) =
  let empties = getEmptySquares board
      neighborMap = makeNeighborMap rank
   in solveSudokuImpl (getEmptySquares board) neighborMap (pruneBoard board empties)

getEmptySquares :: Board -> [(Int, Int)]
getEmptySquares (Board rank arr) =
  [(i, j) | i <- [1 .. rank ^ 2], j <- [1 .. rank ^ 2], isEmpty (arr ! (i, j))]

pruneBoard :: Board -> [(Int, Int)] -> Board
pruneBoard board@(Board rank arr) empties =
  let possibles = map (Possible . getPotentialValues board) empties
      arr' = arr // (empties `zip` possibles)
   in Board rank arr'

printBoard :: Board -> IO ()
printBoard board@(Board rank arr) = forM_ [1 .. rank ^ 2] $ \i -> do
  forM_ [1 .. rank ^ 2] $ \j -> do
    let square = arr ! (i, j)
    putStr $ toString square
    putStr " "
    when (j `mod` rank == 0 && j /= rank ^ 2) $ putStr "|"
  putStr "\n"
  when (i `mod` rank == 0 && i /= rank ^ 2) $ do
    forM_ [1 .. rank ^ 2] $ \j -> do
      putStr "--"
      when (j `mod` rank == 0 && j /= rank ^ 2) $ putStr "+"
    putStr "\n"

getRank :: IO Int
getRank = do
  putStrLn "Please enter rank of board"
  read <$> getLine

readRow :: Int -> [((Int, Int), Square)] -> Int -> IO [((Int, Int), Square)]
readRow rank soFar i = do
  line <- getLine
  let filtered = filter (/= '|') line
      nums = words filtered
      maybes = map readMaybe nums
      maybe2square = \case
        Nothing -> Possible $ Set.fromList [1 .. rank ^ 2]
        Just n -> Fixed n
      squares = map maybe2square maybes
      indices = [(i, j) | j <- [1 .. rank ^ 2]]
  if i `mod` rank == 0 && i /= rank ^ 2 then getLine else return ""
  return $ soFar ++ (indices `zip` squares)

readBoard :: IO Board
readBoard = do
  rank <- getRank
  indices <- foldM (readRow rank) [] [1 .. rank ^ 2]
  return $ Board rank $ array ((1, 1), (rank ^ 2, rank ^ 2)) indices
