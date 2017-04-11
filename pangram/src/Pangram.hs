module Pangram (isPangram) where
import Data.Set (Set, empty, insert, size, fromList, member)
import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = isPangram' text empty

isPangram' :: String -> Set Char -> Bool
isPangram' "" used = used == alphabet
isPangram' (c:cs) used = isPangram' cs (maybeInsert (toLower c) used)

maybeInsert :: Char -> Set Char -> Set Char
maybeInsert c set
  | c `member` alphabet = insert c set
  | otherwise = set

alphabet :: Set Char
alphabet = fromList ['a'..'z']
