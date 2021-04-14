module Lista3 where

--1
fold :: (Int -> Int -> Int) -> [Int] -> Int
fold f [x] = x
fold f (x:xs) = f x (fold f xs)

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior x y z = (fold min [x, y, z], fold max [x, y, z])

--2
iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys)
    |x<=y = x: (y:ys)
    |otherwise = y : ins x ys

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (x, y, z) = tuplify3 (iSort [x , y, z])
    where
        tuplify3 :: [Int] -> (Int, Int, Int)
        tuplify3 [x, y, z] = (x, y, z)

--3
type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

primeiraCoord :: Ponto -> Float
primeiraCoord (x, y) = x

segundaCoord :: Ponto -> Float
segundaCoord (x, y) = y

isVertical :: Reta -> Bool
isVertical (p1, p2) = (primeiraCoord p1) == (primeiraCoord p2)

--4
--não sei testar
pontoY :: Float -> Reta -> Float
pontoY x (p1, p2) = (y2-y1)/(x2-x1) * (x-x1) + y1
    where
        x1 = primeiraCoord p1
        y1 = segundaCoord p1
        x2 = primeiraCoord p2
        y2 = segundaCoord p2