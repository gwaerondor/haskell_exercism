module Beer (song) where
import Data.Char (toUpper, toLower)

song :: String
song = concat [line n | n <- (reverse [0..99])]

line :: Int -> String
line n = (capitalize $ firstPart n) ++ (capitalize $ secondPart n)

firstPart :: Int -> String
firstPart n = nbob n ++ " on the wall, " ++ nbob n ++ ".\n"

nbob :: Int -> String
nbob n = count n ++ " of beer"

secondPart :: Int -> String
secondPart 0 = "Go to the store and buy some more, " ++ nbob 99 ++ " on the wall.\n"
secondPart n = "Take " ++ article n ++ " down and pass it around, " ++ nbob (n-1) ++ " on the wall.\n\n"

count :: Int -> String
count 0 = "No more bottles"
count 1 = "1 bottle"
count n = show n ++ " bottles"

article :: Int -> String
article 1 = "it"
article _ = "one"

capitalize :: String -> String
capitalize (x:xs) = toUpper x:(map toLower xs)
