
module Day23 where

import Computer (Instruction, Reg(..), Computer(Computer, rs), run, parseInstruction, toggle )
import Utils ( getLines )
import Data.Map ( Map, (!), empty, insert )


init7, init12, init12fac :: Map Reg Int
init7 = insert D 0 (insert C 0 (insert B 0 (insert A 7 empty)))
init12 = insert D 0 (insert C 0 (insert B 0 (insert A 12 empty)))
init12fac = insert D 0 (insert C 0 (insert B 1 (insert A 479001600 empty)))


day23 :: IO ()
day23 = do
  ls <- getLines 23
  let is = parseInstruction  <$> ls
      com1 = Computer init7 is 0 []
      newIns :: [Instruction]
      newIns = (\(ix,i) -> if (ix==20) || (ix==22) || (ix==24) then toggle i else i) <$> zip [0..] is
      com2 = Computer init12 is 0 []
      com2fac = Computer init12fac newIns 11 []

  putStrLn $ "Day23: part1: " ++ show (rs (run (const True) com1) ! A)
  putStrLn $ "Day23: part2: " ++ show (rs (run (const True) com2fac) ! A)

  return ()

