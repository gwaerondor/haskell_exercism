module Series (slices) where
import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices 0 _ = [[]]
slices n xs = slices' n intList
  where
    intList = map digitToInt xs

slices' _ [] = []
slices' n xs
  | length xs < n = []
  | otherwise = (take n xs):(slices' n $ tail xs)
