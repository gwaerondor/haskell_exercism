module WordCount (wordCount) where
import Data.Char (toLower)

wordCount :: String -> [(String, Int)]
wordCount xs = count [] $ map normalize $ splitWords xs

normalize :: String -> String
normalize cs = removeWrappingQuotes $ lower cs

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

splitWords :: String -> [String]
splitWords [] = []
splitWords sentence = nextWord:(splitWords remainder)
  where
    nextWord = (takeWhile isWord sentence)
    remainder = dropWhile isSpecial $ dropWhile isWord sentence
    isWord x = not $ isSpecial x
    isSpecial x = x `elem` special
    special = ", !@%^&\n.$:"

lower :: String -> String
lower = map toLower

removeWrappingQuotes :: String -> String
removeWrappingQuotes cs =
  let isWrapped = ((head cs) == '\'') && ((last cs) == '\'') in
  case isWrapped of True -> tail $ init cs
                    False -> cs
