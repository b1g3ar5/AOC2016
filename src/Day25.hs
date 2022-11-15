module Day25 where
import Computer

import System.TimeIt
import Utils
import Data.Map (Map, insert, empty)
import qualified Data.Map as M
import Data.Bifunctor


initA :: Int -> Map Reg Int
initA x = insert D 0 (insert C 0 (insert B 0 (insert A x empty)))


day25 :: IO ()
day25 = do
  ls <- getLines 25
  let is = parseInstruction <$> ls
      comp i  = Computer (initA i) is 0 []
      cs = (\i -> (i, comp i)) <$> [0..200]

  timeIt $ putStrLn $ "Day25: part1: " ++ show (fst $ head $ filter (\t -> [0,1,0,1,0,1,0,1,0,1] == snd t) (second (out . run (\c -> 10 == length (out c) )) <$> cs))
  
  return ()


