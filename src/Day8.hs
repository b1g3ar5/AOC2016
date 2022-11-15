
module Day8 where

import Data.List ( foldl', intercalate )
import Utils ( splitOn, getLines )
import Data.Sequence (Seq(..), (<|), empty, fromList, (><))
import qualified Data.Sequence as S (zip, length, drop, take, filter, replicate)
import Data.Foldable ( Foldable(foldl', toList) )


data Dir = Row | Col deriving (Eq, Show)

data Inst = Rect (Int, Int) | Rotate Dir (Int, Int) deriving (Eq, Show)



startGrid :: Seq (Seq Bool)
startGrid = S.replicate gRows $ S.replicate gCols False
  where
    gCols, gRows :: Int
    gCols = 50
    gRows = 6


transpose :: Seq (Seq a) -> Seq (Seq a)
transpose Empty = empty 
transpose (Empty :<| xss) = transpose xss
transpose ((x :<| xs) :<| xss) = (x <| hs) <| transpose (xs <| ts)  
  where
    hs = (\(h :<| _) -> h) <$> xss
    ts = (\(_ :<| t) -> t) <$> xss


type Grid = Seq (Seq Bool)

showGrid :: Grid -> String
showGrid g = intercalate "\n" $ fmap (\ c -> if c then '#' else '.') . toList <$> toList g


applyInst :: Grid -> Inst -> Grid
applyInst g (Rect (cols, rows)) = (\(r, y) -> (\(c, x) -> (y<rows && x<cols) || c) <$> S.zip r (fromList [0..(S.length r - 1)])) <$> S.zip g (fromList [0..(S.length g - 1)])
applyInst g (Rotate Row (row, n)) = (\(r, y) -> if y==row then S.drop (S.length r - n) r >< S.take (S.length r - n) r else r) <$> S.zip g (fromList [0..(S.length g - 1)])
applyInst g (Rotate Col (col, n)) = transpose $ applyInst (transpose g) (Rotate Row (col, n)) 


day8 :: IO ()
day8 = do
  ls <- getLines 8
  let is :: [Inst]
      is = parseLine <$> ls
  putStrLn $ "Day8: part1:\n" ++ show (sum $ length . S.filter id <$> foldl' applyInst startGrid is)
  putStrLn $ "Day8: part2:\n" ++ showGrid (foldl' applyInst startGrid is)

  return ()


parseLine :: String -> Inst
parseLine s = go ws
  where
    ws = words s
    go (x:y:xs)
      | x == "rect" = Rect (read $ head ys, read $ ys!!1)
      | y == "row" = Rotate Row (read $ drop 2 $ head xs, read $ last xs)
      | y == "column" = Rotate Col (read $ drop 2 $ head xs, read $ last xs)
        where
          ys = splitOn "x" y
