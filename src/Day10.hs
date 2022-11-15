
module Day10 where

import Data.List ( sort, sortOn )
import Utils ( getLines )
import Data.Bifunctor ( Bifunctor(first) )
import Data.Maybe ( fromJust )

type Bix = Int
type Oix = Int

data Bot = Bot { bix:: Bix
               , lo :: Either Oix Bix
               , hi :: Either Oix Bix
               , ilo :: Maybe Bix
               , ihi :: Maybe Bix
               } deriving (Eq, Show)

add :: Int -> Bot -> Bot
add x (Bot a b c Nothing ih) = Bot a b c (Just x) ih
add x (Bot a b c (Just il) ih)
  | x > il = Bot a b c (Just il) (Just x)
  | otherwise = Bot a b c (Just x) (Just il)


parseBot :: String -> Bot
parseBot s = Bot (read $ ws!!1) c1 c2 Nothing Nothing 
  where
    ws = words s
    c1 = (if ws!!5 == "output" then Left else Right) $ read $ ws!!6
    c2 = (if ws!!10 == "output" then Left else Right) $ read $ ws!!11


parseValue :: String -> (Bix, Int)
parseValue s = (read $ ws!!5, read $ ws !!1)
  where
    ws = words s


parse :: [String] -> ([Bot], [(Bix, Int)])
parse = go ([], [])
  where
    go acc [] = acc
    go (bm, vm) ((x:xs):xss)
      | x == 'v' = go (bm, parseValue xs : vm) xss
      | otherwise = go (parseBot xs : bm, vm) xss


-- First apply the values to the bots
applyValues :: [(Bix, Int)] -> [Bot] -> ([(Int, Int)], Bix)
applyValues vs bs = go [] [] bs [] $ first Right <$> vs
  where
    go :: [Bot] -> [Bot] -> [Bot] -> [(Oix, Int)] -> [(Either Oix Bix, Int)] -> ([(Oix, Int)], Bix)
    -- If there are no more values return the output
    go b2 b1 b0 out [] = (out, bix $ head $ filter (\b-> (fromJust (ilo b) == 17) && (fromJust (ihi b) == 61)) b2)
    -- If it's an output value add it to the outputs
    go b2 b1 b0 out ((Left oi, x):vs) = go b2 b1 b0 ((oi, x):out) vs
    -- Now for the value to apply to bots
    go b2 b1 b0 out ((Right bi, x):vs) = 
      case findBot bi b1 of
        -- If it's in b1, turn the bot into a b2 and add the values to the value list
        (Just b, newb1) -> go (add x b : b2) newb1 b0 out $ values (add x b) ++ vs
        _ -> let (Just b, newb0) = findBot bi b0
             in go b2 (add x b : b1) newb0 out vs


-- find a bot in a list of bots
findBot :: Bix -> [Bot] -> (Maybe Bot, [Bot])
findBot bi = go []
  where
    go acc [] = (Nothing, acc)
    go acc (b:bs)
      | bi == bix b = (Just b, acc ++ bs)
      | otherwise = go (b:acc) bs


-- Get the values to apply from a bot
values :: Bot -> [(Either Oix Bix, Int)]
values (Bot _ _ _ Nothing _) = error "This shouldn't happen in values!"
values (Bot _ _ _ _ Nothing) = error "This shouldn't happen in values!"
values (Bot _ lo hi (Just ilo) (Just ihi)) = [(lo, ilo), (hi, ihi)]


day10 :: IO ()
day10 = do
  ls <- getLines 10
  let (bm, vm) = parse ls
      bms = sortOn bix bm
      (pt2, pt1) = applyValues vm bms

  putStrLn $ "day10: part1: " ++ show pt1
  putStrLn $ "day10: part2: " ++ show (product $ snd <$> take 3 (sort pt2))

  return ()


