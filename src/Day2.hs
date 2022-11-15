{-# LANGUAGE  FlexibleInstances #-}

module Day2 where

import Data.List
import qualified Day2a  as D2a (day2a)


getLines :: Int -> IO [String]
getLines = getF lines

getF :: (String -> a) -> Int -> IO a
getF f n = do
  s <- readFile $ "./data/" ++ show n ++ ".in"
  return $ f s


readDir1 :: Char -> Pos1
readDir1 'L' = (-1, 0)
readDir1 'R' = (1, 0)
readDir1 'U' = (0, 1)
readDir1 'D' = (0, -1)
readDir1 c = error $ show c ++ " is not a direction"


type Pos1 = (Int, Int)


bnd :: Int -> Int
bnd x = max ( min x 2) 0


instance Num Pos1 where
  (x1, y1) + (x2, y2) = (bnd $ x1+x2, bnd $ y1+y2)
  (x1, y1) - (x2, y2) = (bnd $ x1-x2, bnd $ y1-y2)
  (x1, y1) * (x2, y2) = (bnd $ x1*x2, bnd $ y1*y2)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger i = (bnd $ fromInteger i, 0)


toInt :: Pos1 -> Int
toInt (0,0) = 7
toInt (1,0) = 8
toInt (2,0) = 9
toInt (0,1) = 4
toInt (1,1) = 5
toInt (2,1) = 6
toInt (0,2) = 1
toInt (1,2) = 2
toInt (2,2) = 3


day2 :: IO ()
day2 = do
  ls <- getLines 2
  let ds1 :: [Pos1]
      ds1 = foldl' (\r@(x:xs) ps -> sum (x:ps) : r) [(1, 1)] $  (readDir1 <$>) <$> ls
  putStrLn $ "Day2: part1: " ++ concatMap show (tail $ reverse $ toInt <$> ds1)
  D2a.day2a

  return ()



test = ["ULL"
  , "RRDDD"
  , "LURDL"
  , "UUUUD"]