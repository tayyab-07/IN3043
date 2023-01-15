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

mapLTree :: (a -> b) -> LTree a -> LTree b
mapLTree f (Leaf x) = Leaf (f x)
mapLTree f (Branch l r) = Branch (mapLTree f l) (mapLTree f r)

size :: LTree a -> Int
size = sumLTree2 . mapLTree (\x -> 1)

depthMap :: LTree a -> Int
depthMap = foldLTree (\ n m -> max n m + 1) . mapLTree (\x -> 0)

listLeavesMap :: LTree a -> [a]
listLeavesMap = foldLTree (++) . mapLTree (\x -> [x])

mirrorMap :: LTree a -> LTree a
mirrorMap = foldLTree (flip Branch) . mapLTree Leaf

data Element = Element Name [Attribute] [Content]
type Name = String
type Attribute = (Name, String)
data Content = Text String | Child Element

printElement :: Element -> String
printElement (Element n attrs []) = "<" ++ unwords (n : map printAttr attrs) ++ "/>"
printElement (Element n attrs body) = "<" ++ unwords (n : map printAttr attrs) ++ ">" ++
concat (map printContent body) ++ "</" ++ n ++ ">"

printContent :: Content -> String
printContent (Text s) = encodeString s
printContent (Child e) = printElement e

printAttr :: Attribute -> String
printAttr (n, val) = n ++ "=\"" ++ encodeString val ++ "\""

data Prop a
   = Var a
   | Not (Prop a)
   | And (Prop a) (Prop a)
   | Imply (Prop a) (Prop a)
   | Or (Prop a) (Prop a)
   | Equiv (Prop a) (Prop a)
   deriving (Show)

mapProp :: (a -> b) -> Prop a -> Prop b
mapProp f (Var v) = Var (f v)
mapProp f (Not p) = Not (mapProp f p)
mapProp f (And p q) = And (mapProp f p) (mapProp f q)
mapProp f (Imply p q) = Imply (mapProp f p) (mapProp f q)
mapProp f (Or p q) = Or (mapProp f p) (mapProp f q)
mapProp f (Equiv p q) = Equiv (mapProp f p) (mapProp f q)

evalBool :: Prop Bool -> Bool
evalBool (Var b) = b
evalBool (Not p) = not (evalBool p)
evalBool (And p q) = evalBool p && evalBool q
evalBool (Imply p q) = not (evalBool p) || evalBool q
evalBool (Or p q) = evalBool p || evalBool q
evalBool (Equiv p q) = evalBool p == evalBool q







