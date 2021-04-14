module Lista3 where

tribonacci :: Int -> Int
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci n = tribonacci (n-1) + tribonacci (n-2) + tribonacci (n-3)

binomial :: Integer->Integer->Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n-1) k + binomial (n-1) (k-1)

poli2 :: Double->Double->Double->Double->Double
poli2 a b c x = a*x^2 + b * x + c

howManyEqual :: Integer->Integer->Integer->Integer
howManyEqual a b c
    |(a==b) && (a==c) = 3
    |(a==b) = 2
    |(a==c) = 2
    |(b==c) = 2
    |otherwise = 0

sumTo :: Integer->Integer
sumTo 0 = 0
sumTo x = sumTo (x-1) + x

maxThree :: Integer->Integer->Integer->Integer
maxThree x y z = max x (max y z)

maxFour :: Integer->Integer->Integer->Integer->Integer
maxFour a b c d
    |maxThree a b c > d = maxThree a b c
    |otherwise = d

maxFour' :: Integer->Integer->Integer->Integer->Integer
maxFour' a b c d = max (max a b) (max c d)

maxFour'' :: Integer->Integer->Integer->Integer->Integer
maxFour'' a b c d = max d (maxThree a b c)

parImpar :: Integer->String
parImpar x
    |par x = "par"
    |otherwise = "impar"

par :: Integer->Bool
par x = if mod x 2 == 0
        then True
        else False

quadruplo :: Integer->Integer
quadruplo x = dobro (dobro x)

dobro :: Integer->Integer
dobro x = 2*x

addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos 1 = " "
addEspacos n = " " ++ addEspacos (n-1)

paraDireita :: Int->String->String
paraDireita n s = addEspacos n ++ s

ehZero :: Int -> Bool
ehZero 0 = True
ehZero n = False



