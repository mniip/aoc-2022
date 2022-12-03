module Main where

import AOC.Common (aocMain)
import Control.Lens (toListOf, both)
import Data.List.Split (chunksOf)
import qualified Data.Set as S

main :: IO ()
main = aocMain readInput1 solve print readInput2 solve print

readInput1 :: String -> [[String]]
readInput1 = map (toListOf both . halves) . lines

halves :: [a] -> ([a], [a])
halves xs = go id xs xs
  where
    go f (x:xs) (_:_:ys) = go (f . (x:)) xs ys
    go f xs _ = (f [], xs)

readInput2 :: String -> [[String]]
readInput2 = chunksOf 3 . lines

priority :: Char -> Int
priority x
  | x >= 'a' = fromEnum x - fromEnum 'a' + 1
  | otherwise = fromEnum x - fromEnum 'A' + 27

solve :: [[String]] -> Int
solve = sum . map priority . concatMap (S.toList . foldr1 S.intersection . map S.fromList)
