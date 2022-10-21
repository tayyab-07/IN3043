module Geometry1 where

data Direction = North | East | South | West

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

