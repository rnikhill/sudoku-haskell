module Main where

import Grid

main :: IO ()
main = do
    board <- readBoard
    let sol = solveSudoku board
    case sol of
        Just b -> printBoard b
        Nothing -> putStrLn "No solution found :("


