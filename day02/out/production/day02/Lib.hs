module Lib
    ( puzzle
    ) where

import System.IO
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.List as List

puzzle :: IO ()
puzzle = do
  handle <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  let tc_ids = ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
      ids = lines content
      tc2_ids = ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
  putStrLn $ "tc_ans1 = " ++ show (checksum tc_ids) ++ " (should be 12)"
  putStrLn $ "ans1 = " ++ show (checksum ids)
  putStrLn $ "tc_ans2 = " ++ show (ans2 tc2_ids) ++ " (should be 'fgij')"
  putStrLn $ "ans2 = " ++ show (ans2 ids)
  hClose handle

checksum :: [String] -> Int
checksum ids = doubles * triples
  where doubles = sumFreq 2
        triples = sumFreq 3
        sumFreq n = sum [1 | f <- freqs, hasFreq n f]
        freqs = map frequency ids
        hasFreq n freqs = elem n (map snd freqs)
        frequency xs = Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs])

ans2 :: [String] -> Maybe String
ans2 [] = Nothing
ans2 (id:ids) = case getCloseMatch id ids of
  Just s -> Just s
  Nothing -> ans2 ids

getCloseMatch :: String -> [String] -> Maybe String
getCloseMatch s ss =
  List.find (isCloseMatch s) commons
  where commons = map (common s) ss
        isCloseMatch s1 s2 = (length s1) == (length s2) + 1
        common s1 s2 = [c | (c,i) <- zip s1 [0..], (s2 !! i) == c]
