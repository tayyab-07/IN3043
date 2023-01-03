module Calender where

isLeapYear :: Int -> Bool
isLeapYear x 
   | (x `mod` 4 == 0) && (x `mod` 400 == 0) = True 
   | otherwise = False

daysInYear :: Int -> Int
daysInYear x 
   | isLeapYear x == True = 366
   | otherwise = 365 

data Month 
   = Jan | Feb | Mar | Apr |
     May | Jun | Jul | Aug |
     Sep | Oct | Nov | Dec
  deriving (Show)

daysInMonth :: Month -> Int -> Int
daysInMonth Jan x = 31
daysInMonth Feb x 
   | isLeapYear x == True = 29
   | otherwise = 28
daysInMonth Mar x = 31
daysInMonth Apr x = 30
daysInMonth May x = 31
daysInMonth Jun x = 30
daysInMonth Jul x = 31
daysInMonth Aug x = 31
daysInMonth Sep x = 30
daysInMonth Oct x = 31
daysInMonth Nov x = 30
daysInMonth Dec x = 31