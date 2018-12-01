module Lib
    ( puzzle
    ) where

import qualified Data.Set as Set
import System.IO

puzzle :: IO ()
puzzle = do
  handle <- openFile "input.txt" ReadMode
  content <- hGetContents handle
  let tc_ints = [1, -2, 3, 1]
      ints = map toInt (lines content)
  print ("ans1 tc = " ++ show (ans1 tc_ints))
  print ("ans2 tc = " ++ show (ans2 tc_ints))
  print ("ans1 = " ++ show (ans1 ints))
  print ("ans2 = " ++ show (ans2 ints))
  hClose handle

ans1 ints = foldl (+) 0 ints

ans2 ints = ans2' (cycle ints) 0 Set.empty
  where ans2' _ pos seen | (Set.member pos seen) = pos
        ans2' (x:xs) pos seen = ans2' xs (pos+x) (Set.insert pos seen)

toInt ('+':s) = read s :: Int
toInt s = read s :: Int
