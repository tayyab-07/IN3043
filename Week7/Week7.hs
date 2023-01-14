module Week7 where

import Data.Char

foo :: Eq a => [a] -> [a]
foo [] = []
foo (x:xs) = x : bar x xs

bar :: Eq a => a -> [a] -> [a]
bar x [] = []
bar x (y:ys)
   | x == y = bar x ys
   | otherwise = y : bar y ys

-- foo takes a list and returns the list but without the adjacent duplicates

removeFirstDigit :: [Char] -> [Char]
removeFirstDigit [] = []
removeFirstDigit (x:xs)
   | isDigit x == True = xs
   | otherwise = x : removeFirstDigit xs

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst f [] = []
removeFirst f (x:xs)
   | f x = xs
   | otherwise = x : removeFirst f xs

addLists :: Num a => [a] -> [a] -> [a]
addLists [] [] = []
addLists xs [] = xs
addLists [] ys = ys
addLists (x:xs) (y:ys) = (x + y) : addLists xs ys 

longZip :: (a -> a -> a) -> [a] -> [a] -> [a]
longZip f [] [] = []
longZip f xs [] = xs
longZip f [] ys = ys
longZip f (x:xs) (y:ys) = (f x y) : longZip f xs ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
   | x < y =  x : merge xs (y:ys)
   | x >= y = y : merge (x:xs) ys

odds :: [a] -> [a]
odds [] = []
odds (x:xs) = x : evens xs

evens :: [a] -> [a]
evens [] = []
evens (y:ys) = odds ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort (x:xs) = merge (mergeSort(odds (x:xs))) (mergeSort(evens (x:xs)))

















