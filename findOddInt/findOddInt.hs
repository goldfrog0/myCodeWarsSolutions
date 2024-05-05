module Codewars.Kata.FindOdd where

import Data.List
import Data.Maybe

findOddIndex :: [Int] -> Int
findOddIndex list = fromJust . elemIndex (snd . head $ aux list) $  list
    where aux list = filter (\x -> odd $ fst x) $ zip [(\x -> length $ filter (==x) list) x | x <- list] list

findOdd :: [Int] -> Int
findOdd list =  snd . head $ aux list
    where aux list = filter (\x -> odd $ fst x) $ zip [(\x -> length $ filter (==x) list) x | x <- list] list          
