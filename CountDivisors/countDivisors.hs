module Divisors where

divisors :: Integral a => a -> Int
divisors x = length [y | y <- [1..x], mod x y == 0 ]
