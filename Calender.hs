module Calender where

isLeapYear :: Int -> Bool
isLeapYear a 
   | a `mod` 400 == 0 = True
   | a `mod` 100 == 0 = False
   | a `mod` 4 == 0 = True
   | otherwise = False

daysInYear :: Int -> Int
daysInYear x = x * 365

data Month =  January | February | March | April | May | June | July | August | September | October | November | December 
   deriving Show

daysInMonth :: Month -> Int -> Int
daysInMonth January x = 31
daysInMonth February x 
   | isLeapYear x = 29 
   | otherwise = 28
daysInMonth March x = 31
daysInMonth April x = 30
daysInMonth May x = 31
daysInMonth June x = 30
daysInMonth July x = 31
daysInMonth August x = 31
daysInMonth September x = 30
daysInMonth October x = 31
daysInMonth November x = 30
daysInMonth December x = 31