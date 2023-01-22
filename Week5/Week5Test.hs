module Week5Test where

import Data.Char

capitalize :: String -> String
capitalize s = map toUpper s

capitals :: String -> String
capitals s = filter isUpper s

capitalize2 :: String -> String
capitalize2 s = filter isAlpha (map toUpper s)

q2a :: [Int]
q2a = map (^2) [1..20]

q2b :: [Int]
q2b = takeWhile (<500) (map (^2) [1..])

q2c :: [Int]
q2c = dropWhile (<500) (takeWhile (<1000) (map (^2) [1..]))

count :: Eq a => a -> [a] -> Int
count x ys = length [y | y <- ys, y == x]
-- returns amount of values in a list that are the same as an input value

count2 :: Eq a => a -> [a] -> Int
count2 x ys = length(filter (==x) ys)

f :: [Integer] -> [Integer]
f = filter ((< 20) . (^2))
-- returns a list with numbers that are less than 20 when squared

fb :: [Float] -> [Float]
fb xs = map (\x -> (x+1)/2) xs

fbSections :: [Float] -> [Float]
fbSections xs = map (/2) ((map (+1) xs)) 

foo :: [Int] -> [Int]
foo xs = zipWith (-) (tail xs) xs
-- returns a list with each value in the list being subtracted from the previous value

collatz :: Int -> Int
collatz x 
   | even x = x `div` 2
   | odd x = (3*x) + 1

collatzSteps :: Int -> Int
collatzSteps x = length (takeWhile (/=1) (iterate collatz x))

collatzMax :: Int -> Int
collatzMax x = maximum (takeWhile (/=1) (iterate collatz x))

pascalRow :: [Int] -> [Int]
pascalRow xs = zipWith (+) ([0] ++ xs) (xs ++ [0])

pascal :: [[Int]]
pascal = take 5 (iterate pascalRow [1])










