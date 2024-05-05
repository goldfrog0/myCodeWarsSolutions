module Codewars.Kata.TenMinuteWalk where

isNonEmpty []    = False
isNonEmpty (_:_) = True

longerThan :: Int -> [a] -> Bool
longerThan n xs = isNonEmpty $ drop n xs

isValidWalk :: [Char] -> Bool
isValidWalk walk 
  | longerThan 10 walk = False
  | length walk == 10  = customVecSum validWalk == [0:: Int, 0:: Int]
  | otherwise          = False
  where
    validWalk = map toPositionVector walk
      
  
toPositionVector :: Char -> [Int] 
toPositionVector 'n' = [0, 1]
toPositionVector 's' = [0, (-1)]
toPositionVector 'w' = [(-1), 0]
toPositionVector 'e' = [1, 0]

customVecSum :: [[Int]] -> [Int]
customVecSum (x:[]) = x
customVecSum (x:y:xs) = customVecSum( (zipWith (+) x y):xs) 


tester = map toPositionVector "nwnwnw"
