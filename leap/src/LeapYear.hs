module LeapYear where

isLeapYear :: Integer -> Bool
isLeapYear year = (divisible year 4) &&
                  (not (divisible year 100) || divisible year 400)

divisible :: Integer -> Integer -> Bool
divisible x y = (mod x y) == 0
