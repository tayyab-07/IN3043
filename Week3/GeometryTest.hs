module Geometry where

-- compass points
data Direction = North | South | East | West
   deriving Show

-- the direction immediately to the left
turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft South = East
turnLeft East = North
turnLeft West = South

-- the direction immediately to the right
turnRight :: Direction -> Direction
turnRight North = East
turnRight South = West
turnRight East = South
turnRight West = North

-- x and y coordinates in two-dimensional space
data Point = Point Int Int
   deriving (Eq, Ord, Show)

-- the origin of the two-dimensional space
origin :: Point
origin = Point 0 0

-- add two points
plusPoint :: Point -> Point -> Point
plusPoint (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)

minusPoint :: Point -> Point -> Point
minusPoint (Point x1 y1) (Point x2 y2) = Point (x1-x2) (y1-y2)

timesPoint :: Int -> Point -> Point
timesPoint a (Point x y) = Point (x*a) (y*a)

normPoint :: Point -> Int
normPoint (Point x y) = (abs x) + (abs y)

distance :: Point -> Point -> Int
distance (Point x1 y1) (Point x2 y2) = normPoint (minusPoint (Point x1 y1) (Point x2 y2))  

oneStep :: Direction -> Point
oneStep North = Point 0 1
oneStep East = Point 1 0
oneStep South = Point 0 (-1)
oneStep West = Point (-1) 0

























