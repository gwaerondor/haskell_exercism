module Series (largestProduct) where
import Data.Char (digitToInt, isDigit) 

largestProduct :: Int -> String -> Maybe Integer
largestProduct size digits
  | isValid = Just (largestProduct' size digits 0)
  | otherwise = Nothing
  where
    isValid = spanIsOk && charsAreValid
    spanIsOk = (size <= length digits) && (size >= 0)
    charsAreValid = all isDigit digits
    
largestProduct' :: Int -> String -> Integer -> Integer
largestProduct' 0 [] _ = 1
largestProduct' size (d:ds) largest
  | size > length (d:ds) = largest
  | otherwise = largestProduct' size ds newLargest
  where
    newLargest = max largest currentProduct
    currentProduct = toInteger $ product $ map digitToInt $ take size (d:ds)
