module Kata (highAndLow) where

import Data.List

--highAndLow :: String -> String
highAndLow "" = "empty list"
highAndLow input = (show $ last sortedList) ++ " " ++ (show $ head sortedList)
  where sortedList = sort $ map read (words input) :: Int
