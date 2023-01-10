module Week5 where

import Data.Char

capitalize :: String -> String
capitalize n = map toUpper n 

capitals :: String -> String
capitals n = filter isUpper n

capitalizeLetters :: String -> String
capitalizeLetters n = filter isAlpha (map toUpper n)

squares20 :: [Int]
squares20 = map (^2) [1..20]

squaresLess500 :: [Int]
squaresLess500 = takeWhile (<500) (map (^2) [1..])

squaresBetween5001000 :: [Int]
squaresBetween5001000 = dropWhile (<500) (takeWhile (<1000) (map (^2) [1..]))

count :: Eq a => a -> [a] -> Int
count x ys = length [y | y <- ys, y == x]

count2 :: Eq a => a -> [a] -> Int
count2 n ns = length (filter (==n) ns)

f :: [Integer] -> [Integer]
f = filter ((< 20) . (^2))
-- The above function "f" squares all the numbers in a list
-- It then outputs the numbers that have a square less than 20

f2 :: [Float] -> [Float]
f2 xs = map (\x -> (x+1)/2) xs

f2changed :: [Float] -> [Float]
f2changed xs = map (/2) (map (+1) xs)

foo :: [Int] -> [Int]
foo xs = zipWith (-) (tail xs) xs
-- The above function shows the sequence in the list of numbers 

collatz :: Int -> Int
collatz n 
   | even n = n `div` 2
   | odd n = (3 * n) + 1

collatzSteps :: Int -> Int
collatzSteps n = length (takeWhile (>1) (iterate collatz n)) 

collatzMax :: Int -> Int
collatzMax n = maximum (takeWhile (>1) (iterate collatz n))

pascalRow :: [Int] -> [Int]
pascalRow x = zipWith (+) ([0] ++ x) (x ++ [0])

pascal :: [[Int]]
pascal = iterate pascalRow [1]

-- scanl (+) 1 [1,3..] 
-- The above expression adds 1 to the first number in the list
-- It then adds the total of that number to the next number in the list

-- scanl (*) 1 [1..]
-- the above expression multiplies a number in the list by the total multiplication of all of the previous values







