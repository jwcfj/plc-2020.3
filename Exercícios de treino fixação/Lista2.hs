module Lista2 where

dobro :: Integer->Integer
dobro x = 2*x

quadruplo :: Integer->Integer
quadruplo x = dobro (dobro x)

poli2 :: Double->Double->Double->Double->Double
poli2 a b c x = a*x^2 + b * x + c

par :: Integer->Bool
par x = if mod x 2 == 0
        then True
        else False

parImpar :: Integer->String
parImpar x
    |par x = "par"
    |otherwise = "impar"

meuMax :: Integer->Integer->Integer
meuMax x y
    |x > y = x
    |otherwise = y

maxThree :: Integer->Integer->Integer->Integer
maxThree x y z = meuMax x (meuMax y z)

maxFour1 :: Integer->Integer->Integer->Integer->Integer
maxFour1 a b c d
    |maxThree a b c > d = maxThree a b c
    |otherwise = d

maxFour2 :: Integer->Integer->Integer->Integer->Integer
maxFour2 a b c d = meuMax (meuMax a b) (meuMax c d)

maxFour3 :: Integer->Integer->Integer->Integer->Integer
maxFour3 a b c d = max d (maxThree a b c)

howManyEqual :: Integer->Integer->Integer->Integer
howManyEqual a b c
    |(a==b) && (a==c) = 3
    |(a==b) = 2
    |(a==c) = 2
    |(b==c) = 2
    |otherwise = 0

--Casamento de padrão: Checa de cima para baixo
ehZero :: Integer->Bool
ehZero 0 = True
ehZero n = False

--underline: Para quando é necessário ver se o elemento existe mas não necessariamente seu valor
ehZero2 :: Integer->Bool
ehZero2 0 = True
ehZero2 _ = False

sumTo :: Integer->Integer
sumTo 1 = 1
sumTo x = sumTo (x-1) + x

potencia :: Integer->Integer->Integer
potencia n 1 = n 
potencia n k = potencia n (k-1) * n

binomial :: Integer->Integer->Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n-1) k + binomial (n-1) (k-1)

--tribonaci :: Integer->Integer
--tribonaci 1 = 1
--tribonaci 2 = 1
--tribonaci n = tribonaci' 1 1 2 (n-2)
--where
--    tribonaci' :: Integer ->Integer ->Integer ->Integer ->Integer
--    tribonaci' a b c 1 = c
--    tribonaci' a b c n = tribonaci'...

addEspacos :: Integer -> String
addEspacos 1 = " "
addEspacos n = " " ++ addEspacos (n-1)

paraDireita :: Integer->String->String
paraDireita n s = addEspacos n ++ s

--14
cabecalho = "Semana   Venda\n"

vendas :: Int -> Int
vendas 0 = 12
vendas 1 = 14
vendas 2 = 15
vendas n = 0

totalVendas :: Int -> Int
totalVendas (-1) = 0
totalVendas n = vendas n + totalVendas (n-1)

mediaVendas :: Int -> Float
mediaVendas n = a / b
    where
        a = fromIntegral (totalVendas n) :: Float
        b = fromIntegral (n + 1) :: Float
        

imprimeSemanas :: Int -> String
imprimeSemanas (-1) = ""
imprimeSemanas n = paraDireita 3 (show(n)) ++ addEspacos 5 ++ show(vendas n) ++ "\n" ++ imprimeSemanas (n-1)

imprimeTotal :: Int -> String
imprimeTotal n = paraDireita 1 "Total" ++ addEspacos 3 ++ show(totalVendas n) ++ "\n"

imprimeMedia :: Int-> String
imprimeMedia n = paraDireita 1 "Média" ++ addEspacos 3 ++ show(mediaVendas n) ++ "\n"

imprimeTabela :: Int -> String
imprimeTabela n = cabecalho
                    ++ imprimeSemanas n
                    ++ imprimeTotal n
                    ++ imprimeMedia n

impressao :: Int -> IO()
impressao n = putStr(imprimeTabela n)
    