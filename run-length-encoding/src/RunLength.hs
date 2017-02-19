module RunLength (decode, encode) where
import Data.Char (isNumber)

decode :: String -> String
decode "" = ""
decode xs = decoded ++ decode rest
  where
    decoded = replicate amount letter
    amount = firstNumber xs
    letter = firstLetter xs
    rest = remaining xs
    
firstNumber :: String -> Int
firstNumber xs = case takeWhile isNumber xs of
                   "" -> 1
                   x -> read x :: Int

firstLetter :: String -> Char
firstLetter = head . dropWhile isNumber

remaining :: String -> String
remaining = tail . dropWhile isNumber

encode :: String -> String
encode "" = ""
encode (x:xs) = encoded ++ (encode rest)
  where
    encoded = reverse (x:(reverse amount))
    rest = dropWhile (== x) xs
    amount = timesRepeated (x:xs)

timesRepeated :: String -> String
timesRepeated (x:xs) = case length (x:(takeWhile (== x) xs)) of
                         1 -> ""
                         n -> show n
