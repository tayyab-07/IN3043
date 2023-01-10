module Turtle where

import Geometry
import Generator

data Turtle = Turtle Point Direction PenState
   deriving Show

data PenState = PenUp | PenDown
   deriving Show

startTurtle :: Turtle 
startTurtle = Turtle origin North PenUp

location :: Turtle -> Point 
location (Turtle point dir pen) = point

data Commands = 
   LeftTurn | RightTurn | Move Int | LiftPen | LowerPen 
   deriving Show

action :: Turtle -> Commands -> Turtle
action (Turtle point dir pen) LeftTurn = Turtle point (turnLeft dir) pen
action (Turtle point dir pen) RightTurn = Turtle point (turnRight dir) pen
action (Turtle point dir pen) (Move x) = Turtle (plusPoint point (timesPoint x (oneStep dir))) dir pen
action (Turtle point dir pen) LiftPen = Turtle point dir PenUp
action (Turtle point dir pen) LowerPen = Turtle point dir PenDown

-- convert a number into a command
genCommand :: Int -> Commands
genCommand n
   | r == 0 = LeftTurn
   | r == 1 = RightTurn
   | r == 2 = LiftPen
   | r == 3 = LowerPen
   | otherwise = Move (r - 3)
   where
      r = n `mod` 11

-- generate an infinite list of commands, for testing
genCommands :: Int -> [Commands]
genCommands seed = map genCommand (generate seed)

turtleHistory :: [Command] -> [Turtle]
turtleHistory = scanl action startTurtle

further :: [Command] -> Int
further cmds =
   length $
   takeWhile (< 400) $
   dropWhile (< 200) $
   map (normPoint . location) $
   turtleHistory cmds