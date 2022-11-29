module Price where

data PriceTag = Item String Double

showPriceTag :: PriceTag -> String
showPriceTag (Item n price) = n ++ " -- " ++ show price

data Shape
   = Circle Double
   | Rectangle Double Double
   deriving (Eq, Show)

area :: Shape -> Double
area (Circle r) = pi*r*r
area (Rectangle w h) = w*h