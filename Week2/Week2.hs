module Week2 where

import Data.Char

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z  
   | x /= y && x /= z && y /= z = True
   | otherwise = False

mystery :: Int -> Int -> Int -> Bool
mystery x y z = not (x == y && y == z)
 -- The mystery function returns True when all 3 numbers are not identical

question3 :: Int -> Int -> Int -> Bool
question3 x y z 
   | x == y && y == z = False
   | otherwise = True

-- Question 4
-- decimal fractions cannot be reprsented with perfect precision. This results in tiny errors in the numbers.

fractionalPart :: Double -> Double
fractionalPart x = x - fromIntegral (floor x)

clampGuards :: Double -> Double -> Double -> Double
clampGuards lo hi x 
   | lo < x && x < hi = x
   | x < lo = lo
   | x > hi = hi

clampMaxMin :: Double -> Double -> Double -> Double
clampMaxMin lo hi x = max lo (min x hi)

charToNum :: Char -> Int
charToNum a 
   | isDigit a = ord a - ord '0' 
   | otherwise = 0






















