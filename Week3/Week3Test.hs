module Week3Test where

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

dup :: a -> (a,a)
dup a = (a,a)

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y  
   | y /= 0 = Just (x `div` y)
   | otherwise = Nothing 

pairMaybe :: Maybe a -> Maybe b -> Maybe (a,b)
pairMaybe (Just a) (Just b) = Just (a,b)
pairMaybe _ _ = Nothing

fromMaybe :: a -> Maybe a -> a
fromMaybe a (Just x) = x
fromMaybe a Nothing = a

whatever :: Either a a -> a
whatever (Left x) = x
whatever (Right y) = y

data Err a = OK a | Error String

both :: Err a -> Err b -> Err (a,b)
both (OK a) (OK b) = OK (a,b)
both (OK a) (Error b) = Error b
both (Error a) (OK b) = Error a
both (Error a) (Error b) = Error (a ++ " " ++ b)