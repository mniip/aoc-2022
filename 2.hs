{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import AOC.Common
import Control.Arrow

main :: IO ()
main = aocMain readInput1 solve1 printOutput1 readInput2 solve2 printOutput2

readInput1 :: _ -> _
readInput1 = map (take2 . words) . lines

readInput2 :: _ -> _
readInput2 = readInput1

printOutput1 :: _ -> IO ()
printOutput1 = print @Int

printOutput2 :: _ -> IO ()
printOutput2 = print @Int

data RPS = R | P | S deriving Eq

solve1 :: _ -> _
solve1 = sum . map (uncurry (+) . ((shape . snd) &&& (win . rate)) . (their *** mine))
  where
    rate (R, P) = GT
    rate (P, S) = GT
    rate (S, R) = GT
    rate (x, y) | x == y = EQ
    rate (_, _) = LT
    their "A" = R
    their "B" = P
    their "C" = S
    mine "X" = R
    mine "Y" = P
    mine "Z" = S
    win LT = 0
    win EQ = 3
    win GT = 6
    shape R = 1
    shape P = 2
    shape S = 3

solve2 :: _ -> _
solve2 = sum . map (uncurry (+) . ((shape . snd) &&& (win . rate)) . choose . (their *** need))
  where
    rate (R, P) = GT
    rate (P, S) = GT
    rate (S, R) = GT
    rate (x, y) | x == y = EQ
    rate (_, _) = LT
    their "A" = R
    their "B" = P
    their "C" = S
    need "X" = GT
    need "Y" = EQ
    need "Z" = LT
    choose (t, w) = (t, head [m | m <- [R, P, S], rate (m, t) == w])
    win LT = 0
    win EQ = 3
    win GT = 6
    shape R = 1
    shape P = 2
    shape S = 3
