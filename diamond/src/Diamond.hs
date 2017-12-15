module Diamond (diamond) where
import Data.Char (ord, chr)

diamond :: Char -> [String]
diamond c = map (generateRow c) charList
  where
    charList = concat [['A'..c], tail [c, chr $ (ord c) - 1..'A']]

generateRow :: Char -> Char -> String
generateRow target c
  | c == 'A'  = sideSpaces ++ "A" ++ sideSpaces
  | otherwise = sideSpaces ++ [c] ++ midSpaces ++ [c] ++ sideSpaces
  where
    sideSpaces = replicate numberOfSideSpaces ' '
    numberOfSideSpaces = (ord target) - (ord c)
    midSpaces = replicate numberOfMidSpaces ' '
    numberOfMidSpaces = ((((ord c) - (ord 'A')) - 1) * 2) + 1
