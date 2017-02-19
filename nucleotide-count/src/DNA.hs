module DNA (nucleotideCounts) where
import Data.Map (Map, insertWith, fromList)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs
  | isValid = Right (foldl (\acc x -> incrementKey acc x) init xs)
  | otherwise = Left "Invalid strand!"
  where
    isValid = all (\x -> x `elem` "ACGT") xs
    init = fromList [('A', 0), ('C', 0), ('G', 0), ('T', 0)]

incrementKey :: Map Char Int -> Char -> Map Char Int
incrementKey m key = insertWith (+) key 1 m
