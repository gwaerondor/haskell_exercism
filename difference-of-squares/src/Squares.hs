module Squares (difference, squareOfSums, sumOfSquares) where

difference :: Integral a => a -> a
difference n = (squareOfSums n) - (sumOfSquares n)

squareOfSums :: (Integral a) => a -> a
squareOfSums n = (^2) $ sum [1..n]

sumOfSquares :: Integral a => a -> a
sumOfSquares n = sum $ map (^2) [1..n]
