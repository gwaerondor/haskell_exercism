module Bob (responseFor) where
import Data.Char (toUpper, isAlpha, isSpace)
import Data.List (reverse, dropWhileEnd)

responseFor :: String -> String
responseFor xs
  | isShouting xs = "Whoa, chill out!"
  | isSilent xs = "Fine. Be that way!"
  | isQuestion xs = "Sure."
  | otherwise = "Whatever."

isShouting :: String -> Bool
isShouting xs = inUpperCase xs == xs && hasLetters xs

isQuestion :: String -> Bool
isQuestion xs = (last . dropTrailingSpaces) xs == '?'
                
isSilent :: String -> Bool
isSilent [] = True
isSilent (x:xs)
  | x `elem` silent = isSilent xs
  | otherwise = False
  where silent = " \r\n\t"

inUpperCase :: String -> String
inUpperCase xs = [ toUpper x | x <- xs ]

hasLetters :: String -> Bool
hasLetters [] = False
hasLetters (x:xs)
  | isAlpha x = True
  | otherwise = hasLetters xs

dropTrailingSpaces :: String -> String
dropTrailingSpaces = dropWhileEnd isSpace
