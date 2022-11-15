
module Day12 where

import Data.Map
import Utils
import Computer

day12 :: IO ()
day12 = do
  ls <- getLines 12
  --let ls = test
  let is = parseInstruction <$> ls
      computer0 = Computer (fromList [(A,0), (B,0), (C,0), (D,0)]) is 0 []
      computer1 = Computer (fromList [(A,0), (B,0), (C,1), (D,0)]) is 0 []
  putStrLn $ "Day12: part1: " ++ show (rs $ run (const True) computer0)
  putStrLn $ "Day12: part2: " ++ show (rs $ run (const True) computer1)

  return ()

test = ["cpy 41 a"
  , "inc a"
  , "inc a"
  , "dec a"
  , "jnz a 2"
  , "dec a"]


