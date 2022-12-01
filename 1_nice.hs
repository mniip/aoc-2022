module Main where

import AOC.Common (aocMain)
import Data.List (foldl', insertBy)
import Data.List.Split (splitOn)

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 print

readInput :: String -> [[Int]]
readInput = map (map read . lines) . splitOn "\n\n"

solve1 :: [[Int]] -> Int
solve1 = maximum . map sum

solve2 :: [[Int]] -> Int
solve2 = sum . top 3 . map sum
  where top n = foldl' (\xs x -> take n $ insertBy (flip compare) x xs) []
