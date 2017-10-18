module Luhn (isValid) where

isValid :: String -> Bool
isValid n
  | length stripped <= 1 = False
  | otherwise = isDivisibleByTen $ calculateLuhnNumber $ toIntList stripped
  where
    stripped = removeSpaces n
    
charToInteger :: Char -> Integer
charToInteger c = read [c]

toIntList :: String -> [Integer]
toIntList cs = map charToInteger $ reverse cs

removeSpaces :: String -> String
removeSpaces cs = filter (/=' ') cs

calculateLuhnNumber :: [Integer] -> Integer
calculateLuhnNumber xs = sum (map subtractIfNecessary $ doubleEverySecond xs)

doubleEverySecond [x] = [x]
doubleEverySecond [] = []
doubleEverySecond (x:y:xs) = (x:(y*2):(doubleEverySecond xs))

subtractIfNecessary x
  | x > 9 = x - 9
  | otherwise = x

isDivisibleByTen x = x `mod` 10 == 0

