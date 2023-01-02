module Week1 where

size :: Integer 
size = 12 + 13 

-- The square of the integer
square :: Integer -> Integer
square n = n * n

-- Triple an integer
triple :: Integer -> Integer
triple n = 3 * n

squareOfTriple :: Integer -> Integer
squareOfTriple n = square (triple n)

secondsInWeek :: Integer
secondsInWeek = 604800

phi :: Double 
phi = (1 + (sqrt 5) / 2)

mile :: Double
mile = 1.609344

milesToKm :: Double -> Double
milesToKm n = n * mile

kmToMiles :: Double -> Double
kmToMiles n = n / mile

question4 :: Integer -> Integer
question4 n = triple (square n)

question5 :: Integer -> Integer
question5 n = square (square n)

factorial :: Int -> Int
factorial n = product [1..n]

--When applying factorial with type Int instead of type Integer sometimes the answers can be too big for Int and they will overflow

norm :: Double -> Double -> Double
norm x y = sqrt ((x^2) + (y^2))
