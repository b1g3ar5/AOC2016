
module Day11 where

-- Part 1
-- To get all the generators to floor 2 = 2*(n-1) = 8
-- To get the (n-1) chips to floor 2 = 2*(n-3)+1 = 5
-- So 13 to get everything to floor 2

-- To get to floor 3:
-- To get all the generators to floor 3 = 2*n = 10
-- To get all the chips to floor 3 = 2*(n-2) + 1 = 7
-- So 17

-- So the whole journey is 13+17+17 = 47

-- Part 2

-- 2*(n-1) = 12
-- 2*(n-3)+1 = 9
-- So 21 to get to floor 2

-- 2 * n = 14
-- 2 * (n-2) + 1 = 11
-- so 25 to get to floor 3

-- So the whole journey is 21+25+25 = 71


day11 :: IO ()
day11 = do

  putStrLn $ "Day11: part1: " ++ show 47
  putStrLn $ "Day11: part2: " ++ show 71

  return ()


