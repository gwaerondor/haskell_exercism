module CryptoSquare (encode) where
import Data.Char (toLower, isAlphaNum)
import Data.List (transpose, intercalate)

encode :: String -> String
encode xs = intercalate " " $ transpose $ chunks columns normalized
  where
    columns = numberOfColumns normalized
    normalized = normalize xs

normalize :: String -> String
normalize = map toLower . filter isAlphaNum

numberOfColumns :: Integral a => String -> a
numberOfColumns xs = ceiling $ sqrt (fromIntegral $ length xs)

chunks :: Int -> [a] -> [[a]]
chunks n xs
  | length xs > n = (take n xs) : (chunks n (drop n xs))
  | otherwise = [xs]
