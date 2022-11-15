{-# LANGUAGE TupleSections #-}

module Day22 where

import Utils hiding (neighbours)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Function

type Node = (Coord, (Int, Int))

type Grid = Map Coord (Int, Int)
data Game = Game { empty :: Coord, target :: Coord, dat :: Coord, grid :: Grid}

parseNode :: String -> Node
parseNode s = ((read $ tail xs, read $ tail ys), (read $ init $ ws!!2, read $ init $ ws!!3))
  where
    ws = words s
    (xs:ys:_) = splitOn "-" $ drop 15 (ws!!0)


viable :: [Node] -> [(Coord, Coord)]
viable = go []
  where
    go acc [] = acc
    go acc ((nc, (nu, na)):ns) = go (acc ++ pairs) ns
      where
        pairs = foldl ho [] ns
        ho acc (mc, (mu, ma))
          | nu>0 && mu>0 && nu<ma && mu<na = acc ++ [(nc, mc), (mc, nc)]
          | nu>0 && nu<ma = acc ++ [(nc, mc)]
          | mu>0 && mu<na = acc ++ [(mc, nc)]
          | otherwise = acc

{-
Start: space is at 4,25 data at (29,0)
Gap in row y=16 is at (0,16)
So the route (of the empty disk) is 
Route: 4,25 -> 0,25 -> 0,15 -> 28,15 -> 28,0
swaps:          4       10      28       15

Then we do the spin move (5 swaps) to get the data to 28,0 and the space to 27,0

So 28*5 = 140 gets the empty disk to 0,0 and the data to 1,0 - then one more swap...

4+10+28+15+140+1 = 198

-}


day22 :: IO ()
day22 = do
  ls <- getLines 22
  let ns = parseNode <$> tail (tail ls)
  putStrLn $ "Day22: part1: " ++ show (length $ viable ns)
  putStrLn "Day22: part2: 198"

  return ()


