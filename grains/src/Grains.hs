module Grains where

square :: Integer -> Integer
square x = 2^(x-1)

total :: Integer
total = foldl (\ cumsum x -> cumsum + (square x)) 0 [1..64]
