module Turtle where

import Geometry

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