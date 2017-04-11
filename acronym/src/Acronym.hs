module Acronym (abbreviate) where
import Data.String (words)
import Data.Char (toUpper, isLower, isUpper)
import Data.Set (Set, member, fromList)

abbreviate :: String -> String
abbreviate xs = map toUpper $ firsts $ toWords $ uncamel xs

uncamel :: String -> String
uncamel (x : y : zs)
  | (isLower x) && (isUpper y) = (x : ' ' : y : uncamel zs)
  | otherwise = (x : uncamel (y:zs))
uncamel zs = zs

toWords :: String -> [String]
toWords text = words $ replaceSpecial text

replaceSpecial :: String -> String
replaceSpecial [] = []
replaceSpecial (x : xs)
  | isSpecial = ' ' : (replaceSpecial xs)
  | otherwise = x : (replaceSpecial xs)
  where
    isSpecial = x `member` (fromList ".-,")

firsts :: [String] -> [Char]
firsts [] = []
firsts ((f : _) : ws) = (f : (firsts ws))
