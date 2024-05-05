module Narcissistic where

narcissistic :: Integral n => n -> Bool
narcissistic n
  | n == narc = True
  | otherwise = False
    where narc = sum .  map (^ length digitsNum ) $ digitsNum
          digitsNum = digits n 
 
    
digits :: Integral a => a -> [a]
digits 0 = []
digits a = digits rest ++ [mod a 10]
  where rest = div a 10  

  
