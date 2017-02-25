module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toLower)

scoreLetter :: Char -> Int
scoreLetter c
  | anyOf "QZ" = 10
  | anyOf "JX" = 8
  | anyOf "K" = 5
  | anyOf "FHVWY" = 4
  | anyOf "BCMP" = 3
  | anyOf "DG" = 2
  | anyOf "AEIOULNRST" = 1
  | otherwise = 0
  where
    anyOf cs = (c `elem` cs) || (c `elem` (map toLower cs))

scoreWord :: String -> Int
scoreWord word = sum $ map scoreLetter word
