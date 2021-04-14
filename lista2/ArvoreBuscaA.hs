-- vamos testar apenas para lista de inteiros
main = interact $ show . buildSearchTree . (read :: String -> [Int])

data Tree t = Node t (Tree t) (Tree t) | Empty
  deriving  (Show, Read)

buildSearchTree :: Ord t => [t] -> Tree t
buildSearchTree xs = insert xs Empty

insert :: Ord t => [t] -> Tree t -> Tree t
insert [] arv = arv
insert (x:xs) Empty = insert xs (Node x (Empty) (Empty))
insert (x:xs) (Node y t1 t2)
    |x < y = insert xs ((Node y (insert [x] t1 ) t2))
    |otherwise = insert xs ((Node y t1 (insert [x] t2)))