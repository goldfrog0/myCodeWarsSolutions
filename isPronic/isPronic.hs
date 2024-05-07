module Codewars.Exercise.Pronic where

isPronic :: Integer -> Bool
isPronic 0 = True
isPronic k = elem k [ n*(n + 1) | n <- [0..k]]
