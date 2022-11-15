
module Day9 where

import Utils ( getRaw )
import Debug.Trace ( trace )


isDigit :: Char -> Bool
isDigit x = x `elem` "0123456789"

pInt :: String -> (Int, String)
pInt = go ""
  where
    go acc rem@(x:xs)
      | isDigit x = go (acc ++ [x]) xs
      | otherwise = (read acc, rem)


parseRep :: String -> ((Int, Int), String)
parseRep s = ((x, y), tail ry)
  where
    (x, rx) = pInt s
    (y, ry) = pInt $ tail rx


parse1 :: String -> Int
parse1 = go 0
  where
    go acc [] = acc
    go acc (x:xs)
      | x=='(' = go (acc + numberOfLetters * repeats) (drop numberOfLetters rem)
      | otherwise = go (acc + 1) xs
        where
          ((numberOfLetters, repeats), rem) = parseRep xs
  

-- This is very slow!
parse2 :: String -> Int
parse2 = go 0
  where
    go acc [] = acc
    go acc (x:xs)
      | x=='(' = go acc (toAdd ++ drop numberOfLetters rem)
      | acc `mod` 10000000 == 0 = trace (show acc) $ go (acc+1) xs
      | otherwise = go (acc + 1) xs
        where
          ((numberOfLetters, repeats), rem) = parseRep xs
          toAdd :: String
          toAdd = concat $ replicate repeats (take numberOfLetters rem)
  


day9 :: IO ()
day9 = do
  ls <- getRaw 9
  putStrLn $ "Day1: part1: " ++ show (parse1 ls)
  putStrLn $ "Day1: part2: " ++ show (parse2 ls) -- 10964557606

