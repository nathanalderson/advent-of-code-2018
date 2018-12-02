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
  where letterFreqs = map frequency ids
        freqFreqs = [frequency (map snd freqs) | freqs <- letterFreqs]
        doubles = sum [1 | f <- freqFreqs, hasFreq 2 f]
        triples = sum [1 | f <- freqFreqs, hasFreq 3 f]

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs])

hasFreq :: Int -> [(Int, Int)] -> Bool
hasFreq n freqFreqs = Maybe.isJust (lookup n freqFreqs)

ans2 :: [String] -> Maybe String
ans2 [] = Nothing
ans2 (id:ids) =
  let closeMatch = fmap (common id) (getCloseMatch id ids)
  in case closeMatch of Just s -> Just s
                        Nothing -> ans2 ids

common :: String -> String -> String
common s1 s2 = [c | (c,i) <- zip s1 [0..], (s2 !! i) == c]

getCloseMatch :: String -> [String] -> Maybe String
getCloseMatch s ss =
  List.find (isCloseMatch s) ss
  where isCloseMatch s1 s2 = length (common s1 s2) == (length s1) - 1

