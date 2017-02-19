module Gigasecond where
import Data.Time (UTCTime, addUTCTime)

fromDay :: UTCTime -> UTCTime
fromDay d = addUTCTime 1000000000 d
