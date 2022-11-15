{-# LANGUAGE TupleSections #-}

module Day13 where

import Prelude hiding (elem)
import qualified Data.Map as M
import Data.Map (Map(..), (!))
import Data.List hiding (elem)
import qualified Data.List as L
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Ord
import Data.Sequence (Seq(..), (|>))
import qualified Data.Vector as V
import Data.Vector (Vector(..))
import Data.Maybe
import Utils hiding (neighbours)

gSpecialNumber = 1350

day13 :: IO ()
day13 = do
  let n = gSize
      start = (1,1)
      end = (31, 39) --(7,4)      
      ns = nodes n
      es = edges ns
      g = (ns, es)
      pruned = prune [start, end] g
      startIx = fromJust $ V.elemIndex start (fst pruned)
      endIx = fromJust $ V.elemIndex end (fst pruned)
      path = pathSearch (startIx, endIx) pruned
  putStrLn $ "Day13: part1: " ++ show (snd path)

  let startIxG = fromJust $ V.elemIndex start (fst g)
      numberOfPaths = countPaths (startIxG, 50) g
  putStrLn $ "Day13: part2: " ++ show numberOfPaths

  return ()

gSize, gCols, gRows :: Int
gSize = 50
gCols = gSize
gRows = gSize


type Grid = Map Coord Bool


gShow :: Int -> String 
gShow n= intercalate "\n" $ (\y -> (\x -> if isWall (x, y) then '#' else '.') <$> [0..n]) <$> [0..n]

isWall :: Coord -> Bool
isWall (x, y) = odd $ length $ filter id b
  where
    i = x*x + 3*x + 2*x*y + y + y*y + gSpecialNumber
    b = fromInt $ fromIntegral i


type Graph = (Vector Coord, Vector [Int])


pathSearch :: (Int, Int) -> Graph -> ([Coord], Int)
pathSearch (start, end) (ns, es) = ((ns V.!) <$> pix, length pix - 1)
  where
    pix = minimumBy (comparing length) $ F.toList <$> go1 (S.singleton start)
    go1 :: Seq Int -> [Seq Int]
    go1 Empty = []
    go1 s@(xs :|> last)
      | last == end = [s]
      | otherwise  = concat $ go1 . (s |>) <$> filter (`notElem` s) (es V.! last)


countPaths :: (Int, Int) -> Graph -> Int
countPaths (start, steps) (ns, es) = length . nub $ concat pix
  where
    pix = F.toList <$> go1 steps (S.singleton start)
    go1 :: Int -> Seq Int -> [Seq Int]
    go1 _ Empty = []
    go1 n s@(xs :|> last)
      | n == 0 = [s]
      | null next = [s]
      | otherwise  = concat $ go1 (n-1) . (s |>) <$> next
      where
        next = filter (`notElem` s) (es V.! last)


elem :: (Eq a) => a -> Seq a -> Bool 
elem x xs = isJust $ S.elemIndexL x xs


prune :: [Coord] -> Graph -> Graph
prune special g 
  | g == h = g
  | otherwise = prune special h
  where
    h = prune1 special g


-- Remove deadends
prune1 :: [Coord] -> Graph -> Graph
prune1 special (ns, es) = (newNodes, newEdges)
  where
    keep :: Int -> Bool
    keep ix = (ns V.! ix `L.elem` special) || length (es V.! ix) > 1
    newNodes = V.ifilter (\ix _ -> keep ix) ns
    -- The new index will be the index of the old index in:
    ixs :: Vector Int
    ixs = V.filter keep $ V.imap const ns
    filteredEdges :: Vector [Int]
    filteredEdges = V.ifilter (\ix _ -> keep ix) es
    newEdges :: Vector [Int]
    newEdges = V.map (\ns -> catMaybes $ (\n -> V.findIndex (==n) ixs) <$> ns) filteredEdges


nodes :: Int -> Vector Coord
nodes n = V.fromList (fst <$> filter snd (concat $ (\y -> (\x -> ((x, y), not $ isWall (x, y))) <$> [0..n]) <$> [0..n]))


edges :: Vector Coord -> Vector [Int]
edges cs = V.imap go cs
  where
    go :: Int -> Coord -> [Int]
    go ix c = catMaybes ((\n -> (==n) `V.findIndex` cs) <$> neighbours c)


neighbours :: Coord -> [Coord]
neighbours (x, y)
  | x==0 && y==0 = [(1, 0),(0, 1)]
  | x==0 = [(1, y),(0, y-1),(0, y+1)]
  | y==0 = [(x-1, 0),(x+1, 0),(x, 1)]
  | x==gSize && y==gSize = [(gSize-1, gSize),(gSize, gSize-1)]
  | x==gSize  = [(gSize-1, gSize),(gSize, gSize-1)]
  | y==gSize = [(gSize-1, gSize),(gSize, gSize-1)]
  | otherwise = [(x-1, y),(x+1, y),(x, y-1),(x, y+1)]



