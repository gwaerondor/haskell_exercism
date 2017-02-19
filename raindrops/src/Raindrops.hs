module Raindrops (convert) where
import Data.List (filter)

convert :: Int -> String
convert n
  | length fs > 0 = toWords fs
  | otherwise = show n
  where
    fs = filter (\x -> n `mod` x == 0) [3, 5, 7]
    
toWords :: [Int] -> String
toWords [] = []
toWords (3:xs) = "Pling" ++ toWords xs
toWords (5:xs) = "Plang" ++ toWords xs
toWords (7:xs) = "Plong" ++ toWords xs

