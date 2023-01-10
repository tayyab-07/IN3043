module ListUtilities where

import Data.List

splits :: [a] -> [([a], [a])]
splits xs = zip (inits xs) (tails xs)

sublists :: [a] -> [[a]]
sublists xs = [ys | ts <- tails xs, ys <- inits ts, not (null ys)]

largestRectangle :: [Int] -> Int
largestRectangle r = maximum [ length x * minimum x | x <- sublists r] 