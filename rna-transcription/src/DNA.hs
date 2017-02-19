module DNA where

toRNA :: String -> String
toRNA xs = [ translate x | x <- xs ]

translate :: Char -> Char
translate 'G' = 'C'
translate 'C' = 'G'
translate 'T' = 'A'
translate 'A' = 'U'
translate _ = 'X'
