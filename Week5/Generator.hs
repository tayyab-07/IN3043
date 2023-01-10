module Generator where

generate :: Int -> [Int]
generate seed = iterate (\ n -> 224149*n + 1) seed