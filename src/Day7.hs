
module Day7 where

import Data.List.Split ( splitOneOf )
import Utils ( getLines )


oddsEvens :: [a] -> ([a], [a])
oddsEvens = go ([],[])
  where
    go :: ([a], [a]) -> [a] -> ([a], [a])
    go (os, es) [] = (os, es)
    go (os, es) [x] = (os++[x], es)
    go (os, es) [x, y] = (os++[x], es++[y])
    go (os, es) (x:y:xs) = go (os++[x], es++[y]) xs


hasABBA :: String -> Bool
hasABBA (a:b:cs) = go a b cs
  where
    go x y [] = False
    go x y [z] = False
    go x y (p:q:rs)
      | x==y = go y p (q:rs)
      | x==q && y==p = True
      | otherwise = go y p (q:rs)
  

ipOK :: ([String], [String]) -> Bool
ipOK (os, es) = or (hasABBA <$> os) && and (not . hasABBA <$> es)


getABA :: String -> [String]
getABA (a:b:c:ds) = go [] a b c ds
  where
    go acc w x y [] = if w /= x && w == y then [w,x,y]:acc else acc
    go acc w x y (z:zs)
      | w /= x && w == y = go ([w,x,y]:acc) x y z zs
      | otherwise = go acc x y z zs


hasSSL :: ([String], [String]) -> Bool
hasSSL (os, es) = or $ or . (\aba -> (\bab -> tail bab == init aba) <$> babs) <$> abas
  where
    abas = concat $ getABA <$> os
    babs = concat $ getABA <$> es


day7 :: IO ()
day7 = do
  ls <- getLines 7
  let oes :: [([String], [String])]
      oes = oddsEvens . splitOneOf "[]" <$> ls

  putStrLn $ "Day7: part1: " ++ show (length $ filter ipOK oes)
  putStrLn $ "Day7: part2: " ++ show (length $ filter hasSSL oes)

  return ()


