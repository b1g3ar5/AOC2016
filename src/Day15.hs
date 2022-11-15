
module Day15 where

import Control.Monad

discSizes :: [Int]
discSizes = [17, 3, 19, 13, 7, 5, 11]


discStarts :: [Int]
discStarts = [15, 2, 4, 2, 2, 0, 0]

series :: Int -> (Int, Int)
series ix = (start, discSizes!!ix)
  where
    start = (discSizes!!ix - discStarts!!ix - (ix+1)) `mod` discSizes!!ix


p1 :: Int -> Int
p1 n = go (head ns) (tail ns) 
  where
    ns :: [(Int, Int)]
    ns = series <$> [0..(n-1)] 
    go :: (Int, Int) -> [(Int, Int)] -> Int
    go (x0, x) [] = x0
    go (x0, x) ((y0, y):yss) = go (go1 0, x*y) yss      
      where
        go1 :: Int -> Int
        go1 k
          | (x0+k*x) `mod` y == y0 = x0+k*x
          | otherwise = go1 (k+1)


day15 :: IO ()
day15 = do
  putStrLn $ "Day15: part1: " ++ show (p1 6)
  putStrLn $ "Day15: part2: " ++ show (p1 7)
  return ()


