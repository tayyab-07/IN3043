module Turle where

import Geometry

data Turtle = Turtle Point Direction PenState
   deriving Show

data PenState = PenUp | PenDown
   deriving Show

data Command = turtleLeft | turtleRight | move Int | liftPen | lowerPen
   deriving show

startTurtle :: Turtle
startTurtle = Turtle origin North PenUp

location :: Turtle -> Point
location (Turtle pos dir pen) = pos

action :: Turtle -> Command -> Turtle
action (Turtle origin North Pen) turtleLeft = Turtle pos (turnLeft dir) pen
action (Turtle origin North Pen) turtleRight = Turtle pos (turnRight dir) pen
action (Turtle origin North Pen) (move n) = Turtle plusPoint pos(timesPoint n(oneStep dir)) dir pen
action (Turtle origin North Pen) listPen = Turtle pos dir PenUp
action (Turtle origin North Pen) listPen = Turtle pos dir PenDown

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y = 
   | y == 0 = Nothing
   | otherwise = Just (div x y)

pairMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
pairMaybe a b = 
   |      
