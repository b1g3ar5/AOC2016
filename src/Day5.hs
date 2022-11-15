{-# LANGUAGE OverloadedStrings #-}

module Day5 where


import Data.List (sort)
--import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.UTF8 as BU
import Crypto.Hash (hashWith, MD5(MD5), hash, SHA256 (..), Digest)
import qualified Data.ByteString as BS


md5 :: BS.ByteString -> BS.ByteString 
md5 msg = BU.fromString (show (hashWith MD5 msg))


hashes1 :: BS.ByteString -> BS.ByteString 
hashes1 pt = go "" 0
  where
    go :: BS.ByteString -> Int -> BS.ByteString
    go acc n
      | BS.length acc == 8 = acc
      | BS.take 5 ct == "00000" = go (acc `BS.append` BS.take 1 (BS.drop 5 ct)) (n+1)
      | otherwise = go acc (n+1)
        where
          ct :: BS.ByteString
          ct = md5 $ pt `BS.append` BU.fromString (show n)
      


hashes2 :: BS.ByteString -> [(Int, Char)]
hashes2 pt = go [] 0
  where
    go :: [(Int, Char)] -> Int -> [(Int, Char)]
    go acc n
      | length acc == 8 = acc
      | BS.take 5 ct == "00000" = case mix of
                                    Just ix -> go (acc ++ [(ix, c1)]) (n+1)
                                    Nothing -> go acc (n+1)
      | otherwise = go acc (n+1)
        where
          ct :: BS.ByteString
          ct = md5 $ pt `BS.append` BU.fromString (show n)
          toIx :: Char -> Maybe Int
          toIx '0' = if 0 `elem` (fst <$> acc) then Nothing else Just 0
          toIx '1' = if 1 `elem` (fst <$> acc) then Nothing else Just 1
          toIx '2' = if 2 `elem` (fst <$> acc) then Nothing else Just 2
          toIx '3' = if 3 `elem` (fst <$> acc) then Nothing else Just 3
          toIx '4' = if 4 `elem` (fst <$> acc) then Nothing else Just 4
          toIx '5' = if 5 `elem` (fst <$> acc) then Nothing else Just 5
          toIx '6' = if 6 `elem` (fst <$> acc) then Nothing else Just 6
          toIx '7' = if 7 `elem` (fst <$> acc) then Nothing else Just 7
          toIx _ = Nothing
          mix = toIx $ head $ BU.toString $ BS.take 1 (BS.drop 5 ct)
          c1 = head $ BU.toString $ BS.take 1 (BS.drop 6 ct)
      

day5 :: IO ()
day5 = do
  let key :: BS.ByteString
      key = "ojvtpuvg"

  putStrLn $ "Day5: part1: " ++ BU.toString (BS.take 8 (hashes1 key))
  putStrLn $ "Day5: part2: " ++ show (snd <$> sort (take 8 (hashes2 key)))
 
  return ()



