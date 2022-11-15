
module Day4 where

import Data.List ( sortBy, group, sort )
import Data.List.Split ( splitOn, wordsBy )
import Data.Char ( ord, chr )

import Utils ( Coord, getLines )


type Room = ([String], Int, String)

parseRoom :: String -> Room
parseRoom s = (init ws, read secId, checkSum)
  where
    ws = wordsBy (=='-') s
    secId:checkSum:cs = splitOn "[" $ init $ last ws


comp :: Ord a => [a] -> [a] -> Ordering
comp x y = if length x == length y then
              head x `compare` head y
              else
                length y `compare` length x


isOK :: Room -> Bool 
isOK (codes, secId, checkSum) = cs == checkSum
  where
    cs :: String
    cs = take 5 $ (head <$> ) <$> sortBy comp $ group $ sort $ concat codes


decode :: Room -> (Int, String)
decode (codes, secId, checkSum) = (secId, unwords $ (decipher <$>) <$> codes)
  where
    decipher :: Char -> Char
    decipher ct = pt
      where
        x = secId `mod` 26
        pt = chr $ ord 'a' + (ord ct - ord 'a' + x) `mod` 26


day4 :: IO ()
day4 = do
  ls <- getLines 4
  let rs :: [Room]
      rs = parseRoom <$> ls
  putStrLn $ "Day4: part1: " ++ show (sum $ (\(_,i,_) -> i) <$> filter isOK rs)
  putStrLn $ "Day4: part2: " ++ show (filter (\(_,s) -> "northpole" `elem` words s) $ decode <$> rs)
  
  return ()

