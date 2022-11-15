module Search where

import Prelude hiding (lookup)
import HeapIndexed (Heap, singleton, union, fromList, extractMin)
import Data.Type.Bool (Not)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

import Utils

type DistMap = Map Coord Dist
type Dist = Maybe Int
-- State is: DistMap, pipeline, visited, finishPred
type State = (DistMap , Heap Dist Coord, Set Coord, Coord -> Bool)


-- Make Nothing represent infinity
-- <> is addition
-- mempty is infinity so that ord/min works
instance {-# overlapping #-} Semigroup Dist where
  Nothing <> Nothing = Nothing 
  Nothing <> _ = Nothing 
  _ <> Nothing = Nothing 
  Just x <> Just y = Just $ x+y


instance {-# overlapping #-} Monoid Dist where
  mempty = Nothing


instance {-# overlapping #-} Ord Dist where
  Nothing <= Nothing = False
  Nothing <= Just _ = False
  Just _ <= Nothing = True
  Just x <= Just y = x <= y


minMaybe :: (Monoid a, Ord a) => Maybe a -> Maybe a -> a
minMaybe Nothing Nothing = mempty
minMaybe (Just x) Nothing = x
minMaybe Nothing (Just x) = x
minMaybe (Just x) (Just y) = min x y


lookup :: Map Coord Dist -> Coord -> Dist
lookup g (x, y)
  | isNothing base = Nothing 
  | otherwise = Just $ 1 + mod (fromJust base + qx + qy - 1) 9
  where
    (qx, rx) = divMod x 100
    (qy, ry) = divMod y 100
    base = g M.! (rx, ry)


-- Global grid sizes
gridSizeX, gridSizeY :: Int
gridSizeX = 100
gridSizeY = 500
inBounds :: (Int, Int) -> Bool
inBounds (x,y) = x>=0 && y>=0 && x < gridSizeX && y < gridSizeY


dijkstra :: State -> Dist
dijkstra (grid, pipeline, visited, finished)
  | null pipeline = mempty
  | finished position = savedMin
  | position `S.member` visited = dijkstra (grid, remainingPipeline, visited, finished)
  | otherwise = dijkstra (grid, newPipeline, S.insert position visited, finished)
  where
    ((savedMin, position), remainingPipeline) = fromJust $ extractMin pipeline
    ns = filter inBounds $ filter (`S.notMember` visited) $ neighbours4 position
    newPipeline = remainingPipeline `union` fromList ((\n -> (savedMin <> (grid `lookup` n ), n)) <$> ns)


