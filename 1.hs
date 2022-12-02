{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import AOC.Common
import Data.List
import Data.List.Split

main :: IO ()
main = aocMain readInput1 solve1 printOutput1 readInput2 solve2 printOutput2

readInput1 :: _ -> [[Int]]
readInput1 = map (map read . lines) . splitOn "\n\n"

readInput2 :: _ -> _
readInput2 = readInput1

printOutput1 :: _ -> IO ()
printOutput1 = print @Int

printOutput2 :: _ -> IO ()
printOutput2 = print @Int

solve1 :: _ -> _
solve1 = maximum . map sum

solve2 :: _ -> _
solve2 = sum . take 3 . reverse . sort . map sum
