
module Day1 where

import qualified Data.Set as S
import Data.List
import Data.List.Split
import Data.Bifunctor

import Utils

type Position = (Turn, Coord)

fn1 :: Position -> (Turn, Int) -> Position
fn1 (t1, cin) (t2, n) = (t1 <> t2, cout)
  where
    cout = cin + bimap (n*) (n*) (toCoord (t1 <> t2))

fn2 :: (Position, Either Coord (S.Set Coord)) -> (Turn, Int) -> (Position , Either Coord (S.Set Coord))
fn2 (p, Left c) _ = (p, Left c)
fn2 ((t1, cin), ecst) (t2, n) = ((t1 <> t2, cout), go ecst $ (cin+) <$> cs)
  where
    cout = cin + bimap (n*) (n*) (toCoord (t1 <> t2))
    cs = (\i -> bimap (i*) (i*) (toCoord (t1 <> t2))) <$> [1..n]
    go :: Either Coord (S.Set Coord) -> [Coord] -> Either Coord (S.Set Coord)
    go acc [] = acc
    go (Left c) _ = Left c
    go (Right st) (c:cs) = case insertIf st c of
                      Nothing -> Left c
                      Just newS -> go (Right newS) cs


insertIf :: Ord a => S.Set a -> a -> Maybe (S.Set a)
insertIf s x = if x `S.member` s then Nothing else Just $ S.insert x s


test = "R8, R4, R4, R8"

day1 :: IO ()
day1 = do
  ws <- getWordsBy (\c -> c==',' || c==' ') 1
  --let ws = ["R8", "R4", "R4","R8"]
  let ns :: [Int]
      ns = read . drop 1 <$> ws
      ts :: [Turn]
      ts = (\[c] -> if c == 'L' then 270 else 90) . take 1 <$> ws
      ret1 = manhattan (0,0) $ snd $ foldl' fn1 (0, (0,0)) $ zip ts ns
      ret2 = foldl' fn2 ((0, (0,0)), Right $ S.singleton (0,0)) $ zip ts ns
  putStrLn $ "Day1: part1: " ++ show ret1
  putStrLn $ "Day1: part2: " ++ show (either (\t -> abs (fst t) + abs (snd t)) (const 999) $ snd ret2)

  return ()


