module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples factors limit = sum $ listOfMultiples factors (limit - 1)

listOfMultiples :: [Int] -> Int -> [Int]
listOfMultiples fs limit = filter (`isMultipleOfAnyFactor` fs) [1..limit]

isMultipleOfAnyFactor :: Int -> [Int] -> Bool
isMultipleOfAnyFactor n fs = any (n `isMultipleOf`) fs

isMultipleOf :: Int -> Int -> Bool
isMultipleOf x y = (x `mod` y) == 0
