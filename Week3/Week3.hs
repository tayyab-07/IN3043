module Week3 where

import Data.Char
-- Week 3 Question 1 and 2

swap :: (a,b) -> (b,a)
swap (a,b) = (b, a)

dup :: a -> (a,a)
dup a = (a,a)

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y  
   | y /= 0 = Just (div x y)
   | otherwise = Nothing

pairMaybe :: Maybe a -> Maybe b -> Maybe (a,b)
pairMaybe (Just a) (Just b) = Just (a,b)
pairMaybe _ _ = Nothing

fromMaybe :: a -> Maybe a -> a 
fromMaybe def Nothing = def
fromMaybe def (Just x) = x

whatever :: Either a a -> a
whatever (Left x) = x
whatever (Right y) = y

data Err a = OK a | Error String

both :: Err a -> Err b -> Err (a,b)
both (OK a) (OK b) = OK (a,b)
both (OK a) (Error msg) = Error msg
both (Error msg) (OK b) = Error msg
both (Error msg1) (Error msg2) = Error (msg1 ++ " " ++ msg2)


-- Week 3 Notes 
data PriceTag = Item String Double

showPriceTag :: PriceTag -> String
showPriceTag (Item n price) =
   n ++ " -- " ++ show price

addVAT :: PriceTag -> PriceTag
addVAT (Item nm price) = Item nm (1.2*price)

data Shape
   = Circle Double
   | Rectangle Double Double
   deriving (Eq, Show)

area :: Shape -> Double
area (Circle r) = pi*r*r
area (Rectangle w h) = w*h

rotate :: Shape -> Shape
rotate (Circle r) = Circle r
rotate (Rectangle w h) = Rectangle h w

scale :: Double -> Shape -> Shape
scale x (Circle r) = Circle (r*x)
scale x (Rectangle w h) = Rectangle (w*x) (h*x)

charToNum :: Char -> Maybe Int
charToNum c
   | isDigit c = Just (ord c - ord '0')
   | otherwise = Nothing

