module Week1 where

-- The square of an integer.
square :: Integer -> Integer
square n = n*n

-- Triple an integer.
triple :: Integer -> Integer
triple n = 3*n

seconds :: Int
seconds = 604800

phi :: Double
phi = (1 + sqrt 5) / 2 

miles :: Double
miles = 1.609344

milesToKm :: Double -> Double
milesToKm m = m * miles

kmToMiles :: Double -> Double
kmToMiles k = k / miles

squareThenTriple :: Integer -> Integer
squareThenTriple x = triple (square x)

squaredSquared :: Integer -> Integer
squaredSquared a = square (square a)

factorial :: Int -> Int
factorial b = product [1..b]

norm :: Double -> Double -> Double
norm x y = sqrt((x*x) + (y*y))








