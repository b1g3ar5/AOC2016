
module Day6 where

import Data.List
import Utils

day6 :: IO ()
day6 = do
  ls <- getLines 6
  let ds1, ds2 :: String
      ds1 = go (negate . length) . group . sort <$> transpose ls
      ds2 = go length . group . sort <$> transpose ls
      go :: (String -> Int) -> [String] -> Char
      go f d = head $ head $ sortOn f d

  putStrLn $ "Day6: part1: " ++ show ds1
  putStrLn $ "Day6: part2: " ++ show ds2

  return ()


