module Codewars.Kata.Next5 where
roundToNext5 :: Int -> Int
roundToNext5 n
  | mod n 5 == 0  = n
  | otherwise     = (+) n $ abs $ mod n 5 - 5
  where lastDigit = mod n 10
