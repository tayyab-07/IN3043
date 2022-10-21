module Week2 where

norm :: Double -> Double -> Double 
norm x y = sqrt (x*x + y*y)


threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = a /= b && b /= c && a /= c

mystery :: Int -> Int -> Int -> Bool
mystery x y z = (x /= y || y /= z || x /= z)

fractionalPart :: Double -> Double
fractionalPart x = x - fromIntegral(floor x)

clamp :: Double -> Double -> Double -> Double
clamp lo hi x 
	| lo < x && x < hi = x
	| x < lo = lo
	| x > hi = hi