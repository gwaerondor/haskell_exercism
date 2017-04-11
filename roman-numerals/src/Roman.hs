module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals x = Just (numerals' x)

numerals' 0 = []
numerals' x
  | x >= 1000 = "M" ++ (numerals' (x-1000))
  | x >= 900 = "CM" ++ (numerals' (x-900))
  | x >= 500 = "D" ++ (numerals' (x-500))
  | x >= 400 = "CD" ++ (numerals' (x-400))
  | x >= 100 = "C" ++ (numerals' (x-100))
  | x >= 90 = "XC" ++ (numerals' (x-90))
  | x >= 50 = "L" ++ (numerals' (x-50))
  | x >= 40 = "XL" ++ (numerals' (x-40))
  | x >= 10 = "X" ++ (numerals' (x-10))
  | x >= 9 = "IX" ++ (numerals' (x-9))
  | x >= 5 = "V" ++ (numerals' (x-5))
  | x >= 4 = "IV" ++ (numerals' (x-4))
  | otherwise = "I" ++ (numerals' (x-1))
