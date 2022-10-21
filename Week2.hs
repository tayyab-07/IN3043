module Week2 where

import Data.Char

norm :: Double -> Double -> Double 
norm x y = sqrt (x*x + y*y)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = a /= b && b /= c && a /= c

mystery :: Int -> Int -> Int -> Bool
mystery x y z = x /= y || y /= z 

fractionalPart :: Double -> Double
fractionalPart x = x - fromIntegral(floor x)

clamp :: Double -> Double -> Double -> Double
clamp lo hi x 
   | lo < x && x < hi = x
   | x < lo = lo
   | x > hi = hi

clampB :: Double -> Double -> Double -> Double
clampB lo hi x = max lo (min x hi)

charToNum :: Char -> Int
charToNum a 
   | isDigit a = ord a - ord '0' 
   | otherwise = 0