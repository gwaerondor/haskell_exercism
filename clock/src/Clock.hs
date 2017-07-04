module Clock (fromHourMin, toString) where

import Text.Printf (printf)

data Clock = Clock Integer deriving (Show, Eq)

instance Num Clock where
  fromInteger m = fromHourMin 0 (wrap m)
  negate (Clock m) = Clock (day - m)
  (+) (Clock m1) (Clock m2) = fromInteger (m1 + m2)

fromHourMin :: Integer -> Integer -> Clock
fromHourMin h m = Clock $ wrap ((h*60) + m)

toString :: Clock -> String
toString (Clock m) = printf "%02d:%02d" hours minutes
  where
    (hours, minutes) = m `divMod` hour

wrap :: Integer -> Integer
wrap minutes = minutes `mod` day

hour :: Integer
hour = 60

day :: Integer
day = 24 * hour
