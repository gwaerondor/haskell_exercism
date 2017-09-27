module Prime (nth) where
import Data.List (find)

nth :: Integer -> Maybe Integer
nth 0 = Nothing
nth n = Just (findNthPrime n 0 1)

findNthPrime :: Integer -> Integer -> Integer -> Integer
findNthPrime targetN currentN number
  | targetN == currentN = number
  | otherwise = findNthPrime targetN (currentN+1) (nextPrime number)

nextPrime :: Integer -> Integer
nextPrime n
  | isPrime $ next = next
  | otherwise = nextPrime next
  where
    next = n + 1

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = all (\x -> n `mod` x /= 0) [2..n-1]
