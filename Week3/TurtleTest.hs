module TurtleTest where

import Geometry

data Turtle = Turtle Point Direction PenState
   deriving (Show)

data PenState = PenUp | PenDown
   deriving (Show) 

startTurtle :: Turtle 
startTurtle = Turtle origin North PenUp 

location :: Turtle -> Point
location (Turtle p d pen) = p

data Commands = 
   LeftTurn | RightTurn | Move Int | LiftPen | LowerPen
   deriving (Show)

action :: Turtle -> Commands -> Turtle
action (Turtle p d pen) LeftTurn = Turtle p (turnLeft d) pen
action (Turtle p d pen) RightTurn = Turtle p (turnRight d) pen
action (Turtle p d pen) (Move n) = Turtle (timesPoint n (oneStep d)) d pen
action (Turtle p d pen) LiftPen = Turtle p d PenUp
action (Turtle p d pen) LowerPen = Turtle p d PenDown