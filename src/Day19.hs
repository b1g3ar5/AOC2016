
module Day19 where

import System.TimeIt ( timeIt )
import Data.Sequence (fromList, Seq(..), (|>), (<|))


steal :: Seq (Int, Int) -> Int
steal ((ix, x) :<| y :<| xs)
  | null xs = ix
  | otherwise = steal $ xs |> (ix, x + snd y)


steal2 :: Bool -> Seq (Int, Int) -> Seq (Int, Int) -> Int
steal2 isOdd ((ix, x) :<| xs) ((iy, y) :<| (iz, z) :<| ys)
  | null xs = iz
  | isOdd =  steal2 (not isOdd) (xs |> (iz, z)) (ys |> (ix, x+y))
  | otherwise = steal2 (not isOdd) xs ((iz, z) <| (ys |> (ix, x+y)))
steal2 _ _ _ = error ""


day19 :: IO ()
day19 = do

  let n = 3014603
      n2 = n `div` 2
      l = zip [1..] $ replicate n 1
  timeIt $ putStrLn $ "Day19: part1: " ++ show (steal $ fromList l)
  timeIt $ putStrLn $ "Day19: part2: " ++ show (steal2 (odd n) (fromList $ take n2 l) (fromList $ drop n2 l))

  return ()


