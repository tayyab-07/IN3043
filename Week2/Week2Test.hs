module Week2Test where

import Data.Char

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z 
   | x /= y && x /= z && z /= y = True
   | otherwise = False

mystery :: Int -> Int -> Int -> Bool
mystery x y z = not (x == y && y == z)
-- returns false if all numbers are the same

mystery2 :: Int -> Int -> Int -> Bool
mystery2 x y z 
   | x == y && y == z = False
   | otherwise = True

fractionalPart :: Double -> Double
fractionalPart x =  x - (fromIntegral (floor x))

clampGuards :: Double -> Double -> Double -> Double
clampGuards lo hi x 
   | lo < x && x < hi = x
   | x < lo = lo
   | x > hi = hi

clampMaxMin :: Double -> Double -> Double -> Double
clampMaxMin lo hi x = min hi (max x lo)

charToNum :: Char -> Int
charToNum a 
   | isDigit a = (ord a) - 48
   | otherwise = 0 

