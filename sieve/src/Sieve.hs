module Sieve (primesUpTo) where

primesUpTo :: Integer -> [Integer]
primesUpTo n = sieve [2..n]

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x:(sieve $ removeMultiples x xs)

removeMultiples :: Integer -> [Integer] -> [Integer]
removeMultiples n xs = filter (\x -> (x `mod` n) /= 0) xs
