module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = reduceToPrimes' [n] [n]

reduceToPrimes' :: [Integer] -> [Integer] -> [Integer]
reduceToPrimes' [] _ = []
reduceToPrimes' (n:ns) init
  | res == init = res
  | otherwise = reduceToPrimes' res res
  where
    res = (reduceToPrimes n (n-1) []) ++ (reduceToPrimes' ns init)

reduceToPrimes :: Integer -> Integer -> [Integer] -> [Integer]
reduceToPrimes remainder currentTry currentFactors
  | remainder < 2 = currentFactors
  | currentTry == 1 = (remainder:currentFactors)
  | remainder /// currentTry = reduceToPrimes newRemainder (newRemainder-1) (currentTry:currentFactors)
  | otherwise = reduceToPrimes remainder (currentTry-1) currentFactors
  where
    newRemainder = remainder `div` currentTry

(///) :: Integral a => a -> a -> Bool
n /// m = n `rem` m == 0
