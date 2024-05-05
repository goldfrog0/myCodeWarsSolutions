module OnesAndZeroes (toNumber) where

toNumber :: [Int] -> Int
toNumber [] = 0
toNumber ints =  sum $ zipWith (*) [2^x | x <- [0..highestDig]] $ reverse ints
  where highestDig = length ints - 1

