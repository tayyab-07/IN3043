data Direction 
   = North | South | East | West 
   deriving (Show)

turnLeft :: Direction -> Direction 
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North 