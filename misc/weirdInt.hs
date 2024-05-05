module WeirdInt where



countBy :: Integer -> Int -> [Integer]
countBy x n = take n [y*x | y <- [1..]]
