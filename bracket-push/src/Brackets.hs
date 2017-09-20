module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = match xs []

match :: String -> [Char] -> Bool
match [] started
  | length started == 0 = True
  | otherwise = False
match (c:cs) started
  | isLeftBracket c = match cs (c:started)
  | (isRightBracket c) && (length started == 0) = False
  | isRightBracket c = case (head started) `isPaired` c of
                         True -> (match cs (tail started));
                         False -> False
  | otherwise = match cs started

isLeftBracket :: Char -> Bool
isLeftBracket c = c `elem` "([{"

isRightBracket :: Char -> Bool
isRightBracket c = c `elem` ")]}"

isPaired :: Char -> Char -> Bool
isPaired '(' ')' = True
isPaired '[' ']' = True
isPaired '{' '}' = True
isPaired _ _ = False
