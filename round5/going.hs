module Codewars.Kata.Suite1 where

going :: Integer -> Double
going n = (1/ fac n') * facSum n where n' = fromIntegral n

fac :: Double -> Double
fac 0 = 1
fac n = n * fac (n - 1)  

facSum :: Integer -> Double
facSum n = sum $ map (fac) [1..n'] where n' = fromIntegral n
