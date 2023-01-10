module Week4 where

import Data.Char
import Data.List

-- Question 1a
-- [1..100]

-- Question 1b
--[n^2 | n <- [1..20]]

--Question 1c
--[n | n <- [1..100], 100 `mod` n == 0]

tripleAll :: [Int] -> [Int]
tripleAll ns = [3*n | n <- ns]

squareAll :: [Int] -> [Int]
squareAll ns = [n^2 | n <- ns]

capitalize :: String -> String
capitalize ns = [toUpper n | n <- ns]

capitalizeLetters :: String -> String
capitalizeLetters ns = [toUpper n | n <- ns, isAlpha n]

backwards :: String -> String
backwards ns = [ n | n <- unwords (reverse (words ns))]

backwards2 :: String -> String
backwards2 ns = [ n | n <- unwords (reverse (words (reverse ns)))]

divisors :: Int -> [Int]
divisors x = [ y | y <- [1..x], x `mod` y == 0]

average :: [Double] -> Double
average nx = (sum nx)/ fromIntegral (length nx)

palindrome :: String -> Bool
palindrome n 
   | n == (reverse n) = True
   | otherwise = False

palindrome2 :: String -> Bool
palindrome2 n 
   | (capitalizeLetters n) == (reverse (capitalizeLetters n)) = True
   | otherwise = False

frequency :: Ord a => [a] -> [(a, Int)]
frequency a = [ (head x, length x) | x <- group(sort a)]

palindromic :: [Char] -> Bool
palindromic p = length [ x | (x, n) <- frequency xs, odd n] <= 1












