{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module AOC.Common
  ( aocMain
  , equals
  , count
  , nubOn
  , deleteOn
  , deleteFirstsOn
  , unionOn
  , intersectOn
  , groupOn
  , groupOn'
  , sortOn
  , insertOn
  , maximumOn
  , minimumOn
  , splitOn2
  , readP
  , median
  , arithSum
  , ceilDiv
  , isqrt
  , ceilIsqrt
  , buildGraph
  , buildGraphThin
  , undirected
  , take2
  , take3
  , take4
  , take5
  , directions4
  , directions8
  , neighbors4
  , neighbors8
  , dijkstra
  , dijkstra'
  , dijkstraTo
  , dijkstraTo'
  , fromBaseBE
  , toBaseBE
  , fromBaseLE
  , toBaseLE
  ) where

import Control.Arrow
import Control.Exception
import Control.Monad
import Data.Array
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function
import Data.Functor.Identity
import Data.Graph
import Data.List
  ( foldl', stripPrefix, sort
  , nubBy, deleteBy, deleteFirstsBy, unionBy, intersectBy, groupBy
  , sortBy, insertBy, maximumBy, minimumBy
  )
import qualified Data.Map as M
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as S
import Data.Ord
import Data.Tuple
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Environment
import System.IO
import qualified Text.ParserCombinators.ReadP as P

class AOCInput i where
  getInput :: IO i

instance AOCInput String where
  getInput = getContents

instance AOCInput BS.ByteString where
  getInput = BS.getContents

instance AOCInput BSL.ByteString where
  getInput = BSL.getContents

instance AOCInput T.Text where
  getInput = T.getContents

instance AOCInput TL.Text where
  getInput = TL.getContents

aocMain
  :: AOCInput i
  => (i -> p1) -> (p1 -> o1) -> (o1 -> IO r1)
  -> (i -> p2) -> (p2 -> o2) -> (o2 -> IO r2)
  -> IO ()
aocMain parse1 solve1 print1 parse2 solve2 print2 = do
  args <- getArgs
  input <- getInput
  when ("1" `elem` args || null args) $ do
    try @SomeException (evaluate $ parse1 input) >>= \case
      Left err -> hPutStr stderr $ "Parsing input for 1: " <> show err
      Right parsed -> try @SomeException (evaluate $ solve1 parsed) >>= \case
        Left err -> hPutStr stderr $ "Solving 1: " <> show err
        Right output -> try @SomeException (print1 output) >>= \case
          Left err -> hPutStr stderr $ "Printing output for 1: " <> show err
          Right _ -> pure ()
  when ("2" `elem` args || null args) $ do
    try @SomeException (evaluate $ parse2 input) >>= \case
      Left err -> hPutStr stderr $ "Parsing input for 2: " <> show err
      Right parsed -> try @SomeException (evaluate $ solve2 parsed) >>= \case
        Left err -> hPutStr stderr $ "Solving 2: " <> show err
        Right output -> try @SomeException (print2 output) >>= \case
          Left err -> hPutStr stderr $ "Printing output for 2: " <> show err
          Right _ -> pure ()

{-# INLINE equals #-}
equals :: Eq a => a -> a -> Bool
equals = (==)

{-# INLINE count #-}
count :: Foldable f => (a -> Bool) -> f a -> Int
count p = foldl' (\n x -> if p x then n + 1 else n) 0

{-# INLINE nubOn #-}
nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f = nubBy (equals `on` f)

{-# INLINE deleteOn #-}
deleteOn :: Eq b => (a -> b) -> a -> [a] -> [a]
deleteOn f = deleteBy (equals `on` f)

{-# INLINE deleteFirstsOn #-}
deleteFirstsOn :: Eq b => (a -> b) -> [a] -> [a] -> [a]
deleteFirstsOn f = deleteFirstsBy (equals `on` f)

{-# INLINE unionOn #-}
unionOn :: Eq b => (a -> b) -> [a] -> [a] -> [a]
unionOn f = unionBy (equals `on` f)

{-# INLINE intersectOn #-}
intersectOn :: Eq b => (a -> b) -> [a] -> [a] -> [a]
intersectOn f = intersectBy (equals `on` f)

{-# INLINE groupOn #-}
groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (equals `on` f)

{-# INLINE groupOn' #-}
groupOn' :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupOn' f = map (f . head &&& id) . groupBy (equals `on` f)

{-# INLINE sortOn #-}
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (comparing f)

{-# INLINE insertOn #-}
insertOn :: Ord b => (a -> b) -> a -> [a] -> [a]
insertOn f = insertBy (comparing f)

{-# INLINE maximumOn #-}
maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = maximumBy (comparing f)

{-# INLINE minimumOn #-}
minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f = minimumBy (comparing f)

{-# INLINE splitOn2 #-}
splitOn2 :: Eq a => [a] -> [a] -> ([a], [a])
splitOn2 needle hay
  | Just xs <- stripPrefix needle hay = ([], xs)
  | h:hs <- hay = first (h:) $ splitOn2 needle hs
  | otherwise = error "splitOn2: separator not found"

readP :: P.ReadP a -> String -> a
readP p xs = case [x | (x, "") <- P.readP_to_S p xs] of
  [x] -> x
  [] -> error "readP: no parse"
  _ -> error "readP: ambiguous parse"

median :: Ord a => [a] -> a
median [] = error "median: empty list"
median xs = sort xs !! (length xs `div` 2)

{-# INLINE arithSum #-}
arithSum :: Integral a => a -> a -> a
arithSum a b = (b * (b + 1) - a * (a - 1)) `div` 2

{-# INLINE ceilDiv #-}
ceilDiv :: Integral a => a -> a -> a
ceilDiv x y = -((-x) `div` y)

{-# SPECIALIZE INLINE isqrt :: Int -> Int #-}
{-# SPECIALIZE INLINE isqrt :: Integer -> Integer #-}
isqrt :: Integral a => a -> a
isqrt n
  | n < 0 = error "isqrt: negative"
  | otherwise = go 0 (n + 1)
  where
    go a b
      | b == a + 1 = a
      | m <= n `div` m = go m b
      | otherwise = go a m
      where m = (a + b) `div` 2


{-# SPECIALIZE INLINE ceilIsqrt :: Int -> Int #-}
{-# SPECIALIZE INLINE ceilIsqrt :: Integer -> Integer #-}
ceilIsqrt :: Integral a => a -> a
ceilIsqrt n
  | n < 0 = error "ceilIsqrt: negative"
  | n == 0 = 0
  | otherwise = go 0 n
  where
    go a b
      | b == a + 1 = b
      | m >= n `ceilDiv` m = go a m
      | otherwise = go m b
      where m = (a + b) `div` 2

mkGraph :: Ord k => [(k, [k])] -> (Graph, Vertex -> k, k -> Vertex)
mkGraph adjs = (graph, out, into)
  where
    (graph, out', into') = graphFromEdges [((), k, ks) | (k, ks) <- adjs]
    !labels = listArray (bounds graph) [k | i <- range (bounds graph), let (_, k, _) = out' i]
    out i = labels ! i
    into k = case into' k of
      Nothing -> error "mkGraph.into: no such vertex"
      Just i -> i

buildGraph :: Ord k => [k] -> [(k, k)] -> (Graph, Vertex -> k, k -> Vertex)
buildGraph vs es = mkGraph $ M.toList
  $ M.fromListWith (<>) $ map (, []) vs <> map (second pure) es

buildGraphThin :: Ord k => [k] -> [(k, k)] -> (Graph, Vertex -> k, k -> Vertex)
buildGraphThin vs es = mkGraph $ map (second S.toList) $ M.toList
  $ M.fromListWith S.union $ map (, S.empty) vs <> map (second S.singleton) es

undirected :: Graph -> Graph
undirected g = buildG (bounds g) $ edges g <> map swap (edges g)

take1 :: [a] -> a
take1 [x] = x
take1 (_:_) = error "take1: expected 1 item, got more"
take1 xs = error $ "take1: expected 1 item, got " <> show (length xs)

take2 :: [a] -> (a, a)
take2 [x, y] = (x, y)
take2 (_:_:_) = error "take2: expected 2 items, got more"
take2 xs = error $ "take2: expected 2 items, got " <> show (length xs)

take3 :: [a] -> (a, a, a)
take3 [x, y, z] = (x, y, z)
take3 (_:_:_:_) = error "take3: expected 3 items, got more"
take3 xs = error $ "take3: expected 3 items, got " <> show (length xs)

take4 :: [a] -> (a, a, a, a)
take4 [x, y, z, w] = (x, y, z, w)
take4 (_:_:_:_:_) = error "take4: expected 4 items, got more"
take4 xs = error $ "take4: expected 4 items, got " <> show (length xs)

take5 :: [a] -> (a, a, a, a, a)
take5 [x, y, z, w, t] = (x, y, z, w, t)
take5 (_:_:_:_:_:_) = error "take5: expected 5 items, got more"
take5 xs = error $ "take5: expected 5 items, got " <> show (length xs)

{-# INLINE directions4 #-}
directions4 :: Num a => [(a, a)]
directions4 = [(0, 1), (1, 0), (0, -1), (-1, 0)]

{-# INLINE directions8 #-}
directions8 :: Num a => [(a, a)]
directions8 = [(0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)]

{-# INLINE neighbors4 #-}
neighbors4 :: Num a => (a, a) -> [(a, a)]
neighbors4 (x, y) = [(x + dx, y + dy) | (dx, dy) <- directions4]

{-# INLINE neighbors8 #-}
neighbors8 :: Num a => (a, a) -> [(a, a)]
neighbors8 (x, y) = [(x + dx, y + dy) | (dx, dy) <- directions8]

runDijkstra
  :: (Monad m, Monoid w, Ord w, Ord a)
  => a
  -> (a -> [(a, w)])
  -> (a -> M.Map a (w, [a]) -> S.Set a -> m ())
  -> m (M.Map a (w, [a]))
runDijkstra init adj hook = go (M.singleton init (mempty, [])) S.empty (PQ.singleton mempty init)
  where
    go weights seen queue = case PQ.minView queue of
      Just (p, queue') -> do
        hook p weights seen
        if p `S.member` seen
        then go weights seen queue'
        else do
          let
            (w, path) = weights M.! p
            (weights', queue'') = foldl' (visit w (p:path)) (weights, queue') (adj p)
          go weights' (S.insert p seen) queue''
      Nothing -> pure weights
    visit w path (weights, queue) (p', w') = case M.lookup p' weights of
      Just (w'', _) | w'' <= w <> w' -> (weights, queue)
      _ -> (M.insert p' (w <> w', path) weights, PQ.insert (w <> w') p' queue)

runDijkstra'
  :: (Monad m, Monoid w, Ord w, Ord a)
  => a
  -> (a -> [(a, w)])
  -> (a -> M.Map a w -> S.Set a -> m ())
  -> m (M.Map a w)
runDijkstra' init adj hook = go (M.singleton init mempty) S.empty (PQ.singleton mempty init)
  where
    go weights seen queue = case PQ.minViewWithKey queue of
      Just ((w, p), queue') -> do
        hook p weights seen
        if p `S.member` seen
        then go weights seen queue'
        else do
          let (weights', queue'') = foldl' (visit w) (weights, queue') (adj p)
          go weights' (S.insert p seen) queue''
      Nothing -> pure weights
    visit w (weights, queue) (p', w') = case M.lookup p' weights of
      Just w'' | w'' <= w <> w' -> (weights, queue)
      _ -> (M.insert p' (w <> w') weights, PQ.insert (w <> w') p' queue)

dijkstraTo :: (Monoid w, Ord w, Ord a) => a -> a -> (a -> [(a, w)]) -> Maybe (w, [a])
dijkstraTo init stop adj = either Just (const Nothing)
  $ runDijkstra init adj $ \p weights _ -> if p == stop
    then Left $ weights M.! p
    else Right ()

dijkstra :: (Monoid w, Ord w, Ord a) => a -> (a -> [(a, w)]) -> M.Map a (w, [a])
dijkstra init adj = runIdentity
  $ runDijkstra init adj $ \_ _ _ -> pure ()

dijkstraTo' :: (Monoid w, Ord w, Ord a) => a -> a -> (a -> [(a, w)]) -> Maybe w
dijkstraTo' init stop adj = either Just (const Nothing)
  $ runDijkstra' init adj $ \p weights _ -> if p == stop
    then Left $ weights M.! p
    else Right ()

dijkstra' :: (Monoid w, Ord w, Ord a) => a -> (a -> [(a, w)]) -> M.Map a w
dijkstra' init adj = runIdentity
  $ runDijkstra' init adj $ \_ _ _ -> pure ()

{-# INLINE fromBaseBE #-}
fromBaseBE :: Int -> [Int] -> Integer
fromBaseBE base = foldl' (\n d -> n * toInteger base + toInteger d) 0

{-# INLINE toBaseBE #-}
toBaseBE :: Int -> Integer -> [Int]
toBaseBE base = go []
  where
    go _ n | n < 0 = error "toBaseBE: negative"
    go ds 0 = ds
    go ds n = case divMod n (toInteger base) of
      (n', d) -> go (fromInteger d:ds) n'

{-# INLINE fromBaseLE #-}
fromBaseLE :: Int -> [Int] -> Integer
fromBaseLE base = fst . foldl' (\(n, p) d -> (n + p * toInteger d, p * toInteger base)) (0, 1)

{-# INLINE toBaseLE #-}
toBaseLE :: Int -> Integer -> [Int]
toBaseLE base = go
  where
    go n | n < 0 = error "toBaseLE: negative"
    go 0 = []
    go n = case divMod n (toInteger base) of
      (n', d) -> fromInteger d:go n'
