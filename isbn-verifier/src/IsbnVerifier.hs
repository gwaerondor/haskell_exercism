module IsbnVerifier (isbn) where
import Data.Char (digitToInt)


isbn :: String -> Bool
isbn cs
  | validInput = isValidIsbn $ digits cs
  | otherwise = False
  where
    validInput = checkDigitIsValid && firstDigitsAreValid
    checkDigitIsValid = ((last cs) `elem` "0123456789X")
    firstDigitsAreValid = (all (`elem` "0123456789-") (init cs))
    
digits :: String -> [Int]
digits [] = []
digits ('-':cs) = digits cs
digits ('X':cs) = 10:(digits cs)
digits (c:cs) = (digitToInt c):(digits cs)
                
isValidIsbn :: [Int] -> Bool
isValidIsbn ds = ((checksum ds 10) `mod` 11) == 0

checksum :: [Int] -> Int -> Int
checksum [] _ = 0
checksum (d:ds) multiplicator = (d * multiplicator) + (checksum ds (multiplicator - 1))
