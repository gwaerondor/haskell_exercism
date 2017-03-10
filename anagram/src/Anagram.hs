module Anagram (anagramsFor) where
import Data.List ((\\))
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (xs `isAnagramOf`) xss
  
isAnagramOf word candidate
  | w == c = False
  | (length w) == (length c) = w \\ c == []
  | otherwise = False
  where
    w = map toLower word
    c = map toLower candidate
