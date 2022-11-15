
module Day3 where

import Data.List ( transpose )
import Data.List.Split ( chunksOf )
import Utils ( getLines )


isTriangle :: [Int]-> Bool
isTriangle ns = (length ns == 3) && (sum ns - mx) > mx
  where
    mx = maximum ns



day3 :: IO ()
day3 = do
  ls <- getLines 3
  let ts :: [[Int]]
      ts = (read <$>) . words <$> ls
      ts2 = concatMap transpose $ chunksOf 3 ts

  putStrLn $ "Day3: part1: " ++ show (length $ filter isTriangle ts)
  putStrLn $ "Day3: part2: " ++ show (length $ filter isTriangle ts2)

  return ()


