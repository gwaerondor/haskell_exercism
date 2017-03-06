module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot Bearing Integer Integer

bearing :: Robot -> Bearing
bearing (Robot bearing _ _) = bearing

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ x y) = (x, y)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot bearing (x, y) = Robot bearing x y

simulate :: Robot -> String -> Robot
simulate robot [] = robot
simulate robot (a:as) = simulate (action a robot) as

action :: Char -> Robot -> Robot
action 'L' (Robot bearing x y) = (Robot (turnLeft bearing) x y)
action 'R' (Robot bearing x y) = (Robot (turnRight bearing) x y)
action 'A' robot = advance robot

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

advance :: Robot -> Robot
advance (Robot North x y) = Robot North x (y + 1)
advance (Robot East x y) = Robot East (x + 1) y
advance (Robot South x y) = Robot South x (y - 1)
advance (Robot West x y) = Robot West (x - 1) y
