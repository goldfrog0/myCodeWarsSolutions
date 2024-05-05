module Persistent where

persistence :: Int -> Int
persistence number
  | number == 0 = 0
  | (length $ digits number) == 1 = 0
  | otherwise = 1 + persistence( foldl (*) 1  $ digits number )


digits :: Integral a => a -> [a]
digits 0 = []
digits a = digits rest ++ [mod a 10]
  where rest = div a 10  
