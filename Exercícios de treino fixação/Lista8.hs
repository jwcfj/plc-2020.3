
--1
import Data.List

data List t =  Nil | Cons t (List t)
    deriving (Eq, Ord, Show)

lengthList :: (List t) -> Int
lengthList (Nil) = 0
lengthList (Cons t l) = 1 + (lengthList l)

findDifference :: (Eq t, Ord t, Show t) => (List t) -> (List t) -> Maybe String
findDifference l1 l2
    |l1 == l2 = Nothing
    |(lengthList l1) /= (lengthList l2) = Just (show (lengthList l1) ++ " /= " ++ show (lengthList l2))
    |otherwise = Just (findDiff l1 l2)

findDiff :: (Eq t, Ord t, Show t) => (List t) -> (List t) -> String
findDiff (Cons t1 l1) (Cons t2 l2)
    |t1 /= t2 = (show t1) ++ " /= " ++ (show t2)
    |otherwise = findDiff l1 l2

--2
data Vetor = Vetor Integer Integer Integer
    deriving Show

instance Eq Vetor where
    (Vetor x1 y1 z1) == (Vetor x2 y2 z2) = ((x1 == x2) && (y1 == y2) && (z1 == z2))

--3
instance Num Vetor where
    (Vetor x1 y1 z1) + (Vetor x2 y2 z2) = (Vetor (x1+x2) (y1+y2) (z1+z2))
    (Vetor x1 y1 z1) - (Vetor x2 y2 z2) = (Vetor (x1-x2) (y1-y2) (z1-z2))
    (Vetor x1 y1 z1) * (Vetor x2 y2 z2) = (Vetor (x1*x2) (y1*y2) (z1*z2))
    negate (Vetor x y z) = (Vetor (negate x) (negate y) (negate z))
    abs (Vetor x y z) = (Vetor (abs x) (abs y) (abs z))
    signum (Vetor x y z) = (Vetor (signum x) (signum y) (signum z))
    fromInteger x = (Vetor x x x)

--4
freqs :: Eq a => [a] -> [(Int, a)]
freqs [] = []
freqs (x:xs) = (contador+1, x) : freqs dif 
    where
        (iguais, dif) = partition (==x) xs
        contador = length iguais

--5
data ITree = ILeaf | INode Int ITree ITree
    deriving Show

instance Eq ITree where
    ILeaf == ILeaf = True
    (INode x te1 td1) == (INode y te2 td2) = ((x==y) && (te1 == te2) && (td1 == td2))
    _ == _ = False

