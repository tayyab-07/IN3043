module Week1Test where

secondsInWeek :: Int
secondsInWeek = 7 * 24 * 60 * 60

phi :: Double
phi = (1 + (sqrt 5)) / 2

mile :: Double
mile = 1.609344

milesToKm :: Double -> Double
milesToKm x = x * mile

kmToMiles :: Double -> Double
kmToMiles x = x / mile

-- The square of an integer.
square :: Integer -> Integer
square n = n*n

-- Triple an integer.
triple :: Integer -> Integer
triple n = 3*n

squareTriple :: Integer -> Integer
squareTriple x = triple (square x)

squareOfSquare :: Integer -> Integer
squareOfSquare x = square (square x)

factorial :: Integer -> Integer
factorial x = product [1..x]

norm :: Double -> Double -> Double
norm x y = sqrt ((x^2) + (y^2))