module WordCount (wordCount) where
import Data.String (words)

wordCount :: String -> [(String, Int)]
wordCount xs = count [] $ words xs

count :: Eq a => [(a, Int)] -> [a] -> [(a, Int)]
count acc [] = acc
count acc (x:xs) = case (lookup x acc) of
                     Nothing ->
                       count ((x, 1):acc) xs;
                     Just n ->
                       count (update x (n+1) acc) xs

update :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
update key newValue ((x, oldValue):xs)
  | x == key = (x, newValue):xs
  | otherwise = (x, oldValue):(update key newValue xs)
