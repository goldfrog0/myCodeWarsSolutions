module MexicanWave where
import Data.Char


wave :: String -> [String]
wave "" = [""]
wave (x:xs) = applyAtN toUpper index x

  


applyAtN :: (a -> a) -> Int -> [a] -> [a]
applyAtN f _ []     = []
applyAtN f 0 (x:xs) = f x : xs
applyAtN f n (x:xs) = x : applyAtN f (n-1) xs
