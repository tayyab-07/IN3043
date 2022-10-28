module Week4 where

import Data.Char

tripleAll :: [Int] -> [Int]
tripleAll ns = [3*n | n <- ns]

squareAll :: [Int] -> [Int]
squareAll ns = [n*n | n <- ns]

capitalize :: String -> String
capitalize str = [toUpper(n) | n <- str]

capitalizeLetters :: String -> String
capitalizeLetters str = [toUpper(n) | n <- str, isAlpha(n)]

backwards :: String -> String
backwards str = unwords [reverse(n) | n <- words str]

