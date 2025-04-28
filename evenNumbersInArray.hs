-- |

module EvenNums where

evenNumbers :: [Int] -> Int -> [Int]
evenNumbers nums n = take n $ reverse $ filter even nums
