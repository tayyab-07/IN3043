module Week8Test where

data LTree a = Leaf a | Branch (LTree a) (LTree a)
   deriving (Show)

tree1 :: LTree Int
tree1 = Branch (Leaf 1) (Leaf 2)

tree2 :: LTree Int
tree2 = Branch (Leaf 2) (Branch (Leaf 3) (Leaf 7))

size :: LTree a -> Int
size (Leaf x) = 1
size (Branch x y) = size x + size y

depth :: LTree a -> Int
depth (Leaf x) = 0
depth (Branch x y) = (depth x `max` depth y) + 1  

listLeaves :: LTree a -> [a]
listLeaves (Leaf x) = [x]
listLeaves (Branch x y) = listLeaves x ++ listLeaves y

mirror :: LTree a -> LTree a
mirror (Leaf x) = Leaf x
mirror (Branch x y) = Branch (mirror y) (mirror x)

sumLTree :: Num a => LTree a -> a
sumLTree (Leaf x) = x
sumLTree (Branch l r) = sumLTree l + sumLTree r

foldLTree :: (a -> a -> a) -> LTree a -> a
foldLTree f (Leaf a) = a
foldLTree f (Branch x y) = f (foldLTree f x) (foldLTree f y)

sumLTree2 :: Num a => LTree a -> a
sumLTree2 = foldLTree (+) 

mapLTree :: (a -> b) -> LTree a -> LTree b
mapLTree f (Leaf x) = Leaf (f x)
mapLTree f (Branch l r) = Branch (mapLTree f l) (mapLTree f r)

sizeMap :: LTree a -> Int
sizeMap = sumLTree . mapLTree (\x -> 1)

depthMap :: LTree a -> Int
depthMap = foldLTree (\ x y -> x `max` y + 1) . mapLTree (\x -> 0)

leavesMap :: LTree a -> [a]
leavesMap = foldLTree (\ x y -> x ++ y) . mapLTree (\x -> [x])

mirrorMap :: LTree a -> LTree a
mirrorMap = foldLTree (\ x y -> Branch y x) . mapLTree (Leaf)





