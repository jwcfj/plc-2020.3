import Data.Either
--1
segDiv :: Int  -> Int -> Maybe Int
segDiv n m
    |(m /= 0) = Just (n `div` m)
    |otherwise = Nothing

--2
segDiv' :: Int  -> Int -> Either [Char] Int
segDiv' n m
    |(m /= 0) = Right (n `div` m) 
    |otherwise = Left (show n ++ "/0")

--3
--mapMaybe :: (a -> Maybe b) -> [a] -> [a]
--mapMaybe f xs = filter (mapMaybeAux f) xs

--mapMaybeAux :: (a -> Maybe b) -> a -> Bool
--mapMaybeAux f a = (f a) /= Nothing
                
--4
classifica :: [Either a b] -> ([a], [b])
classifica xs = (lefts xs, rights xs)

--5
data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Show, Eq)
--a
valAtRoot :: Tree a -> Maybe a
valAtRoot Leaf = Nothing
valAtRoot (Node a t1 t2) = Just a
--b
howManyNodes :: Tree a -> Int
howManyNodes Leaf = 0
howManyNodes (Node a t1 t2) = 1 + (howManyNodes t1) + (howManyNodes t2)
--c
leftest :: Eq a => Tree a -> Maybe a
leftest Leaf = Nothing
leftest (Node a t1 t2)
    |t1 == Leaf = Just a
    |otherwise = leftest t1
--d
mapTree :: (a -> a) -> Tree a -> Tree a
mapTree f Leaf = Leaf
mapTree f (Node a t1 t2) = (Node (f a) (mapTree f t1) (mapTree f t2))
--e
insertL :: a -> Tree a -> Tree a
insertL x Leaf = (Node x (Leaf) (Leaf))
insertL x (Node a t1 t2) = insertL x t1
--f
medida :: Tree a -> Tree Int
medida (Node a Leaf Leaf) = (Node 1 (Leaf) (Leaf))
medida (Node a t1 Leaf) = (Node (1+valor(medida(t1))) (medida t1) (Leaf))
medida (Node a Leaf t2) = (Node (1+valor(medida(t2))) (Leaf) (medida t2))
medida (Node a t1 t2) = (Node (1+valor(medida(t1))+valor(medida(t2))) (medida t1) (medida t2))

valor :: Tree Int -> Int
valor (Node n t1 t2) = n
--g

--h
treeSum :: Tree Int -> Int
treeSum Leaf = 0
treeSum (Node n t1 t2) = n+treeSum(t1)+treeSum(t2)