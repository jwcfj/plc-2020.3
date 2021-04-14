module Lista4 where
import Data.Char

--1
paraMaiuscula :: String -> String
paraMaiuscula xs = [toUpper y| y <- xs]

paraMaiuscula2 :: String -> String
paraMaiuscula2 xs = [toUpper y| y <- xs, isAlpha y]

--2
divisores :: Int -> [Int]
divisores n
    |n<=0 = []
    |otherwise = [ x | x <-[1 .. n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime n = [1, n] == divisores n
--isPrime n = length(divisores n) == 2

--3
menorLista :: [Int] -> Int
menorLista [x] = x
menorLista (x:xs)
    |x < menorLista xs = x
    |otherwise = menorLista xs

--4
cabecalho = "n fib n\n"

imprimeLinha :: Int -> String
imprimeLinha (-1) = ""
imprimeLinha n = imprimeLinha (n-1) ++ show(n) ++ " " ++ show(fib n) ++ "\n"

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

fibTable :: Int -> String
fibTable n = cabecalho ++ imprimeLinha n

impressaoFib :: Int -> IO()
impressaoFib n = putStr(fibTable n)

--5
measure :: [Int] -> Int
measure [] = -1
measure xs = length xs

--6
takeFinal :: Int -> [Int] -> [Int]
takeFinal 0 xs = xs
takeFinal n xs = drop ((length xs) - n) xs

--7
remove :: Int -> [Int] -> [Int]
remove 0 (x:xs) = xs
remove n (x:xs) = [x] ++ remove (n-1) xs

--8
f1 :: [Int] -> Int
f1 [] = 0
f1 (x:xs) = (x+1)

f2 :: [Int] -> Int
f2 (x:xs)
    |length (x:xs) == 0 = 0
    |otherwise = (x+1)

--9
f3 :: [Int] -> Int
f3 [] = 0
f3 [x] = x
f3 (x:xs) = (x + (head xs))

f4 :: [Int] -> Int
f4 (x:xs)
    |length (x:xs) == 0 = 0
    |length (x:xs) == 1 = x
    |otherwise = x + (head xs)

--10
produto :: [Int] -> Int
produto [] = 1
produto (x:xs) = x * (produto xs)

--11
unique :: [Int] -> [Int]
unique [] = []
unique [x] = [x]
--unique (x:xs) =

--12
isCrescente :: [Int] -> Bool
isCrescente [] = True
isCrescente [x] = True
isCrescente (x:xs)
    |x <= (head xs) = True && isCrescente xs
    |otherwise = False