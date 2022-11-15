
module Day18 where

import Utils ( getLines )

rule :: Char -> Char -> Char -> Char
rule '^' '^' '.' = '^'
rule '.' '^' '^' = '^'
rule '^' '.' '.' = '^'
rule '.' '.' '^' = '^'
rule _ _ _ = '.'


next :: String -> String
next (x:xs) = go "" '.' x xs
  where
    go bf left at [] = bf ++ [rule left at '.']
    go bf left at (rt:xs) = go (bf ++ [rule left at rt]) at rt xs


count :: String -> Int
count = go 0
  where
    go n [] = n
    go n (x:xs) = go (n + if x=='.' then 1 else 0) xs


safeTotal :: String -> Int -> Int
safeTotal l n = go 0 l n
  where
    go tot l n
      | n == 1 = tot + count l
      | otherwise = go (tot + count l) (next l) (n-1)


day18 :: IO ()
day18 = do
  l:ls <- getLines 18
  putStrLn $ "Day18: part1: " ++ show (safeTotal l 40)
  putStrLn $ "Day18: part2: " ++ show (safeTotal l 400000)

  return ()


