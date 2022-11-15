

module Day16 where

import Data.List (find)
import System.TimeIt ( timeIt )
import Data.Fix


-- Maybe this could be a hylomorphism?
-- anamorphism to build the disk
-- catamorphism to work out eh checksum

-- The disk could be a binary tree - the middle '0' (or '1') is the node
-- and the sides are either the input or the reverse not of the input.

-- The leaf is either the input or not reveerse input
type Label = Bool

data DiskF r = Leaf Label | Node r r

type Disk = Fix DiskF


disk :: String -> String
disk s = s ++ ['0'] ++ (reverse . map invert $ s)
    where invert c = if c == '0' then '1' else '0'


checkSum :: String -> String
checkSum [] = []
checkSum (x:y:zs) = bit x y : checkSum zs
    where bit a b = if a == b then '1' else '0'


fill :: Int -> String -> Maybe String
fill l x = f x >>= (g . take l)
    where f = find ((> l) . length) . iterate disk
          g = find (odd . length) . iterate checkSum

day16 :: IO ()
day16 = do
  let input = "01110110101001000"
  putStrLn $ "Day16: part1: " ++ show (fill 272 input)
  putStrLn $ "Day16: part2: " ++ show (fill 35651584 input)


