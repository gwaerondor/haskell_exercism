module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay Teenth weekday year month = getTeenth weekday year month 13
meetupDay schedule weekday year month = error "You need to implement this function."

getTeenth :: Weekday -> Integer -> Int -> Int -> Day
getTeenth weekday year month day
  | isCorrectDay = fromGregorian year month day
  | otherwise = getTeenth weekday year month (day+1)
  where
    calendarDay = fromGregorian year month day
    isCorrectDay = (third (toWeekDate calendarDay)) == (toWeekdayNumber weekday)

toWeekdayNumber :: Weekday -> Int
toWeekdayNumber Monday = 1
toWeekdayNumber Tuesday = 2
toWeekdayNumber Wednesday = 3
toWeekdayNumber Thursday = 4
toWeekdayNumber Friday = 5
toWeekdayNumber Saturday = 6
toWeekdayNumber Sunday = 7

third :: (a, b, c) -> c
third (_, _, x) = x
