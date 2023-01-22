module Week10Test where

import System.Directory

palindrome :: IO ()
palindrome = do 
   putStrLn "Write a Palindrome: "
   p <- getLine
   if p == (reverse p) then 
      putStrLn "That is a palindrome!"
   else do
      putStrLn "That is not a palindorme, try again" 
      palindrome

reverseOrder :: IO ()
reverseOrder = do 
   putStrLn "Enter a file path:"
   fp <- getLine
   s <- readFile fp
   putStrLn (unlines(reverse(lines s)))

currentDirectory :: IO ()
currentDirectory = do 
   f <- getDirectoryContents "."
   putStrLn (unlines f)

printFile :: FilePath -> IO ()
printFile file = do
   contents <- readFile file
   putStr contents

printAll :: IO ()
printAll = do
   files <- getDirectoryContents "."
   sequence_ [printFile file | file <- files]

repeatIO :: Int -> IO a -> IO ()
repeatIO x action = do
   action 
   repeatIO (x-1) action
  





























