{-# LANGUAGE  FlexibleInstances #-}

module Day2a (day2a) where

import Data.List


getLines :: Int -> IO [String]
getLines = getF lines

getF :: (String -> a) -> Int -> IO a
getF f n = do
  s <- readFile $ "./data/" ++ show n ++ ".in"
  return $ f s


readDir2 :: Char -> Pos2
readDir2 'L' = (-1, 0)
readDir2 'R' = (1, 0)
readDir2 'U' = (0, 1)
readDir2 'D' = (0, -1)
readDir2 c = error $ show c ++ " is not a direction"


type Pos2 = (Integer, Integer)


allowed :: Pos2 -> Bool 
allowed c = c `elem` [(0,2), (1,2), (2,2), (3,2), (4,2), (1,3), (2,3), (3,3), (2,4), (1,1), (2,1), (3,1), (2,0)]


instance Num Pos2 where
  (x1, y1) + (x2, y2) = if allowed (x1+x2, y1+y2) then (x1+x2, y1+y2) else (x1, y1)
  (x1, y1) - (x2, y2) = undefined
  (x1, y1) * (x2, y2) = undefined
  abs (x, y) = undefined
  signum (x, y) = undefined
  fromInteger i = (fromInteger i, 0)



toChar :: Pos2 -> Char
toChar (0,2) = '5'
toChar (1,2) = '6'
toChar (2,2) = '7'
toChar (3,2) = '8'
toChar (4,2) = '9'
toChar (1,3) = '2'
toChar (2,3) = '3'
toChar (3,3) = '4'
toChar (2,4) = '1'
toChar (1,1) = 'A'
toChar (2,1) = 'B'
toChar (3,1) = 'C'
toChar (2,0) = 'D'



day2a :: IO ()
day2a = do
  ls <- getLines 2
  let ds2 :: [Pos2]
      ds2 = foldl' (\r@(x:xs) ps -> sum (x:ps) : r) [(0, 2)] $  (readDir2 <$>) <$> ls
  putStrLn $ "Day2: part2: " ++ show (tail $ reverse $ toChar <$> ds2)

  return ()

