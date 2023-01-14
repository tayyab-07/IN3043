module Week8 where

data LTree a = Leaf a | Branch (LTree a) (LTree a)
   deriving (Show)

tree1 :: LTree Int
tree1 = Branch (Leaf 2) (Leaf 3)

tree2 :: LTree Int
tree2 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)

sizeTree :: LTree a -> Int
sizeTree (Leaf x) = 1
sizeTree (Branch x y) = sizeTree x + sizeTree y

depth :: LTree a -> Int
depth (Leaf x) = 0
depth (Branch x y) = (max (depth x) (depth y)) + 1

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
foldLTree f (Leaf x) = x
foldLTree f (Branch x y) = f (foldLTree f x) (foldLTree f y)

sumLTree2 :: Num a => LTree a -> a
sumLTree2 = foldLTree (+)























