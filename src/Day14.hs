{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day14 where

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS hiding (groupBy)
import qualified Data.ByteString.UTF8 as BU
import Data.ByteString.Char8 (pack)
import Crypto.Hash ( hashWith, MD5(MD5), Digest(..), digestFromByteString, hashDigestSize, hash )
import Data.Maybe ( fromJust, fromMaybe, isJust )
import Data.Bifunctor ( Bifunctor(first) )
import System.TimeIt ( timeIt )
import Data.List ( group, groupBy, nub, sort )
import Control.Monad ( guard )


safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:xs) = Just x


gSalt :: String
--gSalt = "abc"
gSalt = "jlmsuwbz"


md5 :: String -> String
md5 s = show (hash $ pack s :: Digest MD5)


md5n :: String -> String
md5n s = foldr (\_ h -> md5 h) (md5 s) [1..2016]


hashes :: [String]
hashes = md5 . (\i -> gSalt ++ show i) <$> [0..40000]

stretchedHashes :: [String]
stretchedHashes = md5n . (\i -> gSalt ++ show i) <$> [0..40000]


-- The first 3-peat for each ix
h3s :: [String] -> M.Map Char [Int]
h3s hashes = M.fromList $ (\g -> (fst $ head g, snd <$>g)) <$> gps
  where
    gps :: [[(Char, Int)]]
    gps = groupBy (\a b -> fst a == fst b) $ sort $ first fromJust <$> kix
    kix :: [(Maybe Char, Int)]
    kix = filter (\(mc, i) -> isJust mc) $ first (\h -> safehead $ head <$> filter (\x -> length x >= 3) (group h)) <$> zip hashes [0..]


-- All 5-peats for each index
h5s :: [String] -> M.Map Char [Int]
h5s hashes = M.fromList $ (\g -> (fst $ head g, snd <$> g)) <$> gps
  where
    gps = groupBy (\a b -> fst a == fst b) $ sort kix
    kix :: [(Char, Int)]
    kix = concatMap (\(s, i) -> (, i) <$> s) ksix
    ksix :: [(String, Int)]
    ksix = filter (\(s, _) -> not $ null s) $ first (\h -> head <$> filter (\x -> length x >= 5) (group h)) <$> zip hashes [0..]


digits :: String
digits = ['0'..'9'] ++ ['a'..'f']


keys :: [String] -> [Int]
keys hashes = sort $ concatMap go digits
  where
    go :: Char -> [Int]
    go d = do
      i3 <- fromMaybe [] $ d `M.lookup` h3s hashes
      i5 <- fromMaybe [] $ d `M.lookup` h5s hashes
      guard (i5 <= i3 + 1000 && i5 > i3)
      return i3


day14 :: IO ()
day14 = do
  putStrLn $ "Day14: part1: " ++ show (take 64 $ nub $ take 80 $ keys hashes)
  putStrLn $ "Day14: part2: " ++ show (take 64 $ nub $ take 80 $ keys stretchedHashes)

  return ()




-------------- GRAVEYARD

{-


-- Get the next n-repeated characters and the ix that they occur at
findRepeats :: Int -> Int -> (String, Int)
findRepeats repeats ix
  | null rs = findRepeats repeats $ ix + 1
  | otherwise = (take 1 rs, ix)
  where
    rs = getn repeats ix


findRepeatsOf :: Char -> Int -> (Int, Int) -> Maybe Int
findRepeatsOf c repeats (n, end)
  | n > end = Nothing
  | c `elem` rs = Just n
  | otherwise = findRepeatsOf c repeats (n+1 , end)
  where
    rs = getn repeats n


-- Get all n-repeated characters in the string
getn :: Int -> Int -> String
getn n i = head <$> filter (\x -> length x >= n) (group $ hashes !! i)


-- Get the next key
getKey :: Int -> Int
getKey start = if null c5 then getKey (ix3+1) else ix3
  where
    --(c3, ix3) = findRepeats 3 start
    --c5 = catMaybes $ (\c -> findRepeatsOf c 5 (ix3+1, ix3+1000)) <$> c3
    (c3, ix3) = findRepeats 3 start
    c5 = catMaybes $ (\c -> findRepeatsOf c 5 (ix3+1, ix3+1000)) <$> c3


-- Get a number of keys
getKeys :: Int -> BS.ByteString -> Int -> [Int]
getKeys keyRequired salt start = tail $ reverse $ go [0] 
  where
    go keys
      | length keys == (keyRequired+1) = keys
      | otherwise = go (nextIx : keys)
      where
        nextIx = getKey $ 1 + head keys



-}