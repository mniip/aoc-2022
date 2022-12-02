{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import AOC.Common
import Control.Arrow

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 print

readInput :: String -> [(String, String)]
readInput = map (take2 . words) . lines

data RPS = Rock | Paper | Scissors deriving Enum

decodeOpponent :: String -> RPS
decodeOpponent = \case
  "A" -> Rock
  "B" -> Paper
  "C" -> Scissors

decodeMine :: String -> RPS
decodeMine = \case
  "X" -> Rock
  "Y" -> Paper
  "Z" -> Scissors

decodeResult :: String -> Ordering
decodeResult = \case
  "X" -> LT
  "Y" -> EQ
  "Z" -> GT

solve1 :: [(String, String)] -> Int
solve1 = sum . map (score . (fromEnum . decodeOpponent *** fromEnum . decodeMine))
  where score (o, m) = 3 * ((1 - o + m) `mod` 3) + 1 + m

solve2 = sum . map (score . (fromEnum . decodeOpponent *** fromEnum . decodeResult))
  where score (o, r) = 3 * r + 1 + ((o - 1 + r) `mod` 3)
