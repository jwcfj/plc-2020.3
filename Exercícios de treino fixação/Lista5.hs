module Lista5 where

import Data.Char
--1
mLength :: [Int] -> Int
mLength xs = sum(map paraUm xs)

paraUm :: t -> Int
paraUm n = 1

--2
uppers :: String -> String
uppers str = map toUpper str

doubles :: [Int] -> [Int]
doubles xs = map dobro xs

dobro :: Int -> Int
dobro x = 2*x

centavosReais :: [Int] -> [Float]
centavosReais xs = map paraReais xs

paraReais :: Int -> Float
paraReais x = (fromIntegral  x) / (fromIntegral 100)

--3
letras :: String -> String
letras str = filter isAlpha str

rmChar :: Char -> String -> String
rmChar ch str = filter (/=ch) str

acima :: Int -> [Int] -> [Int]
acima n xs = filter (>n) xs

desiguais :: [(Int, Int)] -> [(Int,Int)]
desiguais xs = filter tuplaNEqual xs

tuplaNEqual :: (Int, Int) -> Bool
tuplaNEqual (x, y) = x /= y

--4
--a
f1 :: String -> String
f1 str = map toUpper(filter isAlpha str)
--b
f2 :: [Int] -> [Int]
f2 xs = map dobro (filter (>3) xs)
--c
f3 :: [String] -> [String]
f3 strs = map reverse (filter f3Aux strs)
f3Aux :: String -> Bool
f3Aux str = even (length str)

--5
--a
productRec :: [Int] -> Int
productRec xs = productFold xs
productFold :: [Int] -> Int
productFold (x:xs) = foldr (*) x xs
--b
andRec :: [Bool] -> Bool
andRec bs = andFold bs
andFold :: [Bool] -> Bool
andFold (b:bs) = foldr (&&) b bs
--c
concatRec :: [String] -> String
concatRec strs = concatFold strs
concatFold :: [String] -> String
concatFold (str:strs) = foldl (++) str strs