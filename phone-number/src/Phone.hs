module Phone (number) where
import Data.Char (isNumber)

number :: String -> Maybe String
number pn
  | valid = Just (removeCountryCode $ clean)
  | otherwise = Nothing
  where
    valid = onlyValidInput pn && validLength clean
    clean = filter (\x -> isNumber x) pn

onlyValidInput :: String -> Bool
onlyValidInput pn = all (\x -> x `elem` "1234567890() -.") pn

validLength :: String -> Bool
validLength ('1':pn) = (length pn == 10) || (length pn == 9)
validLength pn = length pn == 10

removeCountryCode :: String -> String
removeCountryCode ('1':pn)
  | length pn == 10 = pn
  | otherwise = ('1':pn)
removeCountryCode pn = pn
