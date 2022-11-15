{-# LANGUAGE OverloadedStrings #-}

module Day17 where

import Data.ByteString.Char8 (pack)
import Crypto.Hash ( hashWith, MD5(MD5), Digest(..), digestFromByteString, hashDigestSize, hash )

import Alg ( hylo, Algebra, Coalgebra, ListF(..), TreeF(..) )
import Utils ( Coord, Dir(..) )
import System.TimeIt ( timeIt )
import Data.List ( minimumBy )
import Data.Ord ( comparing )
import Data.Function ( on )


md5 :: String -> String
md5 s = show (hash $ pack s :: Digest MD5)


gSalt, gSalt1, gSalt2, gSalt3 :: String
gSalt = "edjrjqaa"
gSalt1 = "ihgpwlah"
gSalt2 = "kglvqrro"
gSalt3 = "ulqzkmiv"

seed :: String
seed = gSalt

gSize :: Int
gSize = 3

type Path = ([Dir], Coord)


open :: String
open = "bcdef"

showDir :: Dir -> Char
showDir N = 'U'
showDir S = 'D'
showDir E = 'R'
showDir W = 'L'


directionsAvailable :: Path -> [Dir]
directionsAvailable ds = snd <$> filter (\(c, d) -> c `elem` open) (zip cs [N, S, W, E])
  where
    cs = take 4 $ md5 $ seed ++ (showDir <$> fst ds)


gExit ::Coord
gExit = (gSize, gSize)


day17 :: IO ()
day17 = do
  timeIt $ putStrLn $ "Day17: part1: " ++ show (hylo alg coalg [([S], (0,1)),([E],(1,0))])
  timeIt $ putStrLn $ "Day17: part2: " ++ show (hylo lengthAlg coalg [([S], (0,1)),([E],(1,0))])
  timeIt $ putStrLn $ "Day17: part2: " ++ show (hylo algTree coalgTree ([], (0,0)))
  timeIt $ putStrLn $ "Day17: part2: " ++ show (hylo lengthAlgTree coalgTree ([], (0,0)))
 
  return ()


neighbours :: Coord -> [Dir]
neighbours (x, y)
  | x==0 && y==0 = [S,E]
  | x==gSize && y==gSize = [N, W]
  | x==0 && y==gSize = [N,E]
  | x==gSize && y==0 = [S, W]
  | x==0 = [E, N, S]
  | y==0 = [S, W, E]
  | x==gSize  = [W, N, S]
  | y==gSize = [N, W, E]
  | otherwise = [N,S,E,W]


move :: Coord -> Dir -> Coord
move (x, y) N = (x, y - 1)
move (x, y) S = (x, y + 1)
move (x, y) W = (x - 1, y)
move (x, y) E = (x + 1, y)


coalg :: Coalgebra (ListF Path) [Path]
coalg [] = NilF
coalg (x@(ds, loc):xs)
  | loc == gExit = ConsF x xs
  | otherwise = ConsF x (xs ++ ((\d -> (ds ++ [d], move loc d)) <$> nexts))
  where
    nexts = filter (\d -> d `elem` neighbours loc) $ directionsAvailable x


lengthAlg :: Algebra (ListF Path) Int
lengthAlg NilF = 0
lengthAlg (ConsF (p,l) n)
  | l == gExit = max (length p) n
  | otherwise = n


alg :: Algebra (ListF Path) (Maybe String)
alg NilF = Nothing
alg (ConsF x@(p,l) xs)
  | l == gExit =  Just $ showDir <$> p
  | otherwise = xs


-- Tree where each node has the location and the path to get there
-- This is necessary because we need the path to work out which doors are open
coalgTree :: Coalgebra (TreeF Path) Path
coalgTree x@(ds, loc)
  | loc == gExit = NodeF x []
  | otherwise = NodeF x ((\d -> (ds ++ [d], move loc d)) <$> nexts)
  where
    nexts = filter (\d -> d `elem` neighbours loc) $ directionsAvailable x

lengthAlgTree :: Algebra (TreeF Path) Int
lengthAlgTree (NodeF (p,l) ns)
  | l == gExit = maximum $ length p : ns
  | null ns = 0
  | otherwise = maximum ns

algTree :: Algebra (TreeF Path) (Maybe String)
algTree (NodeF (p,l) ns)
  | l == gExit = Just $ showDir <$> p
  | null ns = Nothing
  | otherwise = minimumBy (compare `on` mlen) ns
  where
    mlen Nothing = 100000 -- lazy!
    mlen (Just s) = length s