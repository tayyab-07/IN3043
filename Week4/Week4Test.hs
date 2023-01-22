module Week4Test where

import Data.Char
import Data.List

-- 1a)
-- [n | n <- [1..100]]

-- 1b)
-- [ n^2 | n <- [1..20]]

-- 1c)
-- [ n | n <- [1..100], 100 `mod` n == 0]

squareAll :: [Int] -> [Int]
squareAll xs = [ n^2 | n <- xs]

capitalize :: String -> String
capitalize s = [ toUpper n | n <- s]

capitalizeLetters :: String -> String
capitalizeLetters s = [ toUpper n | n <- s, isAlpha n]

backwards :: String -> String
backwards s = [n | n <- unwords(reverse(words s))]

backwards2 :: String -> String
backwards2 s = [n | n <- unwords (reverse (words (reverse s)))]

divisors :: Int -> [Int]
divisors x = [n | n <- [1..x], x `mod` n == 0]

average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)

palindrome :: String -> Bool
palindrome s 
   | s == reverse s = True
   | otherwise = False

palindrome2 :: String -> Bool
palindrome2 s = palindrome (capitalizeLetters s)

frequency :: Ord a => [a] -> [(a, Int)]
frequency s = [ (head a, length a) | a <- group(sort s)]

palindromic :: [Char] -> Bool
palindromic s =  length [ b | (a,b) <- frequency s, odd b] <= 1
























