
module Day20 where

import Utils (getLines, splitOn)
import Data.List (sortBy)
import Data.Function (on)
import Data.Sequence (Seq(..), (|>), (<|), fromList)
import Data.Foldable (toList)

type Range = (Int, Int)

readRange :: String -> Range
readRange s = (read $ head xs, read $ head $ tail xs)
  where
    xs = splitOn "-" s


consolidate :: Seq Range -> Seq Range
consolidate = go Empty
  where
    go :: Seq Range -> Seq Range -> Seq Range
    go acc Empty = acc
    go Empty ((l2, h2):<|xs) = go ((l2, h2) <| Empty) xs
    go (acc:|>(l1, h1)) ((l2, h2):<|xs)
      | h1<(l2-1) = go (acc|>(l1,h1)|>(l2,h2)) xs
      | h1<=h2 = go (acc|>(l1,h2)) xs
      | otherwise = go (acc|>(l1,h1)) xs


count :: Seq Range -> Int
count = go (top+1)
  where
    go acc Empty = acc
    go acc ((l,h):<|xs) = go (acc-h+l-1) xs


top :: Int
top = 4294967295

day20 :: IO ()
day20 = do
  ls <- getLines 20
  let ranges = sortBy (compare `on` fst) (readRange <$> ls)
      simple@((l, h) :<| _) = consolidate $ fromList ranges
  putStrLn $ "Day20: part1: " ++ show (1 + h)
  putStrLn $ "Day20: part2: " ++ show (count simple)

  return ()


