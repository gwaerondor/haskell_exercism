module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength)
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
meetupDay Teenth weekday year month = getTeenth weekday year month
meetupDay First weekday year month = getNth weekday year month 1
meetupDay Second weekday year month = getNth weekday year month 2
meetupDay Third weekday year month = getNth weekday year month 3
meetupDay Fourth weekday year month = getNth weekday year month 4
meetupDay Last weekday year month = getLast weekday year month lastDay
  where
    lastDay = gregorianMonthLength year month

getTeenth :: Weekday -> Integer -> Int -> Day
getTeenth weekday year month = getFirstAfter weekday year month 13

getNth :: Weekday -> Integer -> Int -> Int -> Day
getNth weekday year month n = getFirstAfter weekday year month (1+((n-1)*7))

getLast :: Weekday -> Integer -> Int -> Int -> Day
getLast weekday year month day = getFirstBefore weekday year month day

getFirstAfter :: Weekday -> Integer -> Int -> Int -> Day
getFirstAfter weekday year month day = findFirst weekday year month day (+1)

getFirstBefore :: Weekday -> Integer -> Int -> Int -> Day
getFirstBefore weekday year month day = findFirst weekday year month day (+ (-1))

findFirst :: Weekday -> Integer -> Int -> Int -> (Int -> Int) -> Day
findFirst weekday year month day incrementor
  | found = fromGregorian year month day
  | otherwise = findFirst weekday year month (incrementor day) incrementor
  where
    calendarDay = fromGregorian year month day
    found = (third (toWeekDate calendarDay)) == (toWeekdayNumber weekday)

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
