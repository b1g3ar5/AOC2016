
module Day21 where

import Utils ( getLines )
import Data.List ( elemIndex )
import Data.Maybe ( fromMaybe )

data Way = Lt | Rt deriving (Show, Eq)

data Op = SwapC Char Char | SwapP Int Int | RotC Char | RotP Way Int | Rev Int Int | Move Int Int deriving (Show, Eq)

undo :: String -> Op -> String
undo s (SwapC a b) = apply s (SwapC a b)
undo s (SwapP a b) = apply s (SwapP a b)
undo s (RotC c) = apply s (RotP Rt $ mod (inverseIndex cix - cix) $ length s)
  where
    cix = fromMaybe (error $ "Can't find letter: " ++ [c]) (elemIndex c s)
undo s (RotP Rt i) = apply s (RotP Lt i)
undo s (RotP Lt i) = apply s (RotP Rt i)
undo s (Rev a b) = apply s (Rev a b)
undo s (Move a b) = apply s (Move b a)

-- This is to work out the RotC inverse - clunky...
inverseIndex :: Int -> Int
inverseIndex 0 = 7
inverseIndex 1 = 0
inverseIndex 2 = 4
inverseIndex 3 = 1
inverseIndex 4 = 5
inverseIndex 5 = 2
inverseIndex 6 = 6
inverseIndex 7 = 3

parseOp :: [String] -> Op
parseOp ws
  | ws!!1 == "letter" = SwapC (head $ ws!!2) (head $ ws!!5)
  | ws!!0 == "swap" && ws!!1 == "position" = SwapP (read $ ws!!2) (read $ ws!!5)
  | ws!!1 == "based" = RotC (head $ ws!!6)
  | ws!!1 == "right" = RotP Rt (read $ ws!!2)
  | ws!!1 == "left" = RotP Lt (read $ ws!!2)
  | ws!!0 == "reverse" = Rev (read $ ws!!2) (read $ ws!!4)
  | ws!!0 == "move" = Move (read $ ws!!2) (read $ ws!!5)
  | otherwise = error "unidentified operation"


apply :: String -> Op -> String
apply s (SwapC c d) = (\x -> if x == c then d else (if x == d then c else x)) <$> s
apply s (SwapP i j) = snd . (\(ix,x) -> if ix == i then (ix, s!!j) else (if ix == j then (ix, s!!i) else (ix,x))) <$> zip [0..] s
apply s (RotC c) = apply s (RotP Rt $  rem (cix + 1 + if cix>=4 then 1 else 0) $ length s)
  where
    cix = fromMaybe (error $ "Can't find letter: " ++ [c]) (elemIndex c s)
apply s (RotP Rt x) = apply s (RotP Lt $ length s - x)
apply s (RotP Lt x) = drop x s ++ take x s
apply s (Rev x y) = take x s ++ reverse (drop x $ take (y+1) s) ++ drop (y+1) s
apply s (Move x y) = take y remove ++ (s!!x : drop y remove)
  where
    remove = take x s ++ drop (x+1) s



day21 :: IO ()
day21 = do
  ls <- getLines 21
  let ops = parseOp . words <$> ls
      pwd = "abcdefgh"
      code = "fbgdceah"
  putStrLn $ "Day21: part1: " ++ show (foldl apply pwd ops)
  putStrLn $ "Day21: part2: " ++ show (foldl undo code $ reverse ops)

  return ()


