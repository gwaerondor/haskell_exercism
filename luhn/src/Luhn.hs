module Luhn (isValid) where

isValid :: String -> Bool
isValid n = isDivisibleByTen $ sum $ map subtractIfNecessary $ multiplyEverySecond $ map charToInteger $ removeSpaces $ reverse n
  where
    charToInteger c = read [c] :: Integer
    
multiplyEverySecond [x] = [x]
multiplyEverySecond [] = []
multiplyEverySecond (x:y:xs) = (x:(y*2):(multiplyEverySecond xs))

subtractIfNecessary x
  | x > 9 = x - 9
  | otherwise = x

isDivisibleByTen 0 = False
isDivisibleByTen x = x `mod` 10 == 0

removeSpaces cs = filter (/=' ') cs
