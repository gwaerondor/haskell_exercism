module Clock (clockHour, clockMin, fromHourMin, toString) where

import Text.Printf (printf)

data Clock = Clock Integer deriving (Show, Eq)

instance Num Clock where
  fromInteger m = fromHourMin 0 (m `mod` day)
  negate (Clock m) = Clock (day - m)
  (+) (Clock m1) (Clock m2) = fromInteger (m1 + m2)

clockHour :: Clock -> Integer
clockHour (Clock minutes) = first $ (minutes `divMod` hour)
  where
    first (f, _) = f
    
clockMin :: Clock -> Integer
clockMin (Clock m) = second $ (m `divMod` hour)
  where
    second (_, s) = s

fromHourMin :: Integer -> Integer -> Clock
fromHourMin h m = Clock (((h*60) + m) `mod` day)

toString :: Clock -> String
toString (Clock m) = printf "%02d:%02d" hours minutes
  where
    (hours, minutes) = (m `divMod` hour)

hour :: Integer
hour = 60

day :: Integer
day = 24 * hour
