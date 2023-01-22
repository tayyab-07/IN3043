module Week7Test where

import Data.Char
import Data.List

foo :: Eq a => [a] -> [a]
foo [] = []
foo (x:xs) = x : bar x xs

bar :: Eq a => a -> [a] -> [a]
bar x [] = []
bar x (y:ys)
   | x == y = bar x ys
   | otherwise = y : bar y ys

--Foo outputs the same lits in the same order but without the identical values that are stored consecutively in the list

removeFirstDigit :: [Char] -> [Char]
removeFirstDigit [] = []
removeFirstDigit (x:xs) 
   | isDigit x = xs
   | otherwise = x : removeFirstDigit xs

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst f [] = []
removeFirst f (x:xs) 
   | f x = xs
   | otherwise = x : removeFirst f xs

addLists :: Num a => [a] -> [a] -> [a]
addLists [] [] = []
addLists (x:xs) [] = (x:xs)
addLists [] (y:ys) = (y:ys)
addLists (x:xs) (y:ys) = (x + y) : addLists xs ys

longZip :: (a -> a -> a) -> [a] -> [a] -> [a]
longZip f [] [] = []
longZip f (x:xs) [] = (x:xs)
longZip f [] (y:ys) = (y:ys)
longZip f (x:xs) (y:ys) = (f x y) : longZip f xs ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge (x:xs) [] = (x:xs)
merge [] (y:ys) = (y:ys)
merge (x:xs) (y:ys) 
   | x < y = x : merge xs (y:ys)
   | otherwise = y : merge (x:xs) ys

odds :: [a] -> [a]
odds [] = []
odds (x:xs) = x : evens xs

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = odds xs

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort (x:xs) = merge (sort(evens (x:xs))) (sort (odds (x:xs)))












