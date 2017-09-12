module Atbash (decode, encode) where
import Data.Char (toLower)
import Data.Map as Map (fromList, (!)) 
import Data.List (intercalate)

decode :: String -> String
decode cipherText = map invertAlpha $ removeSpecial cipherText

encode :: String -> String
encode plainText = spacify $ map invertAlpha $ removeSpecial $ lower plainText

lower :: String -> String
lower = map toLower

invertAlpha :: Char -> Char
invertAlpha c
  | c `elem` ['a'..'z'] = (Map.fromList $ zip ['a'..'z'] ['z','y'..'a']) ! c
  | otherwise = c

removeSpecial :: String -> String
removeSpecial cs = filter (\x -> not $ x `elem` " ,.") cs

spacify :: String -> String
spacify cx = intercalate " " $ chunks 5 cx

chunks :: Int -> [a] -> [[a]]
chunks n xs
  | length xs > n = (take n xs) : (chunks n (drop n xs))
  | otherwise = [xs]
