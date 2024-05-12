module LettersBestFriend where
import Data.List.Split
bestFriend :: String -> Char -> Char -> Bool
bestFriend "" _ _ = True
bestFriend string point buddy =  and $ map (\x -> helper x point buddy)$ (divvy 2 1 string) ++ [[last string]]


--checks if blocks are valid
helper :: String -> Char -> Char -> Bool 
helper (a:[]) point helper = not (a == point) 
helper (a:b:xs) point helper
  | a == point = b == helper
  | otherwise  = True






