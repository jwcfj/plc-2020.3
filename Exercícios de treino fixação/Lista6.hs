module Lista6 where

import Data.List
import Data.Char

--1
mmax :: (t -> Int) -> t -> t -> t
mmax f x y
    |f x >= f y = x
    |otherwise = y

--2
countSorted :: [String] -> Int
countSorted strs = length (filter sorted strs)
    where sorted s = s == sort s

--3
--mStr :: [String] -> String
--mStr strs = map toUpper . intercalate " \n " (filter aux strs)
--    where aux str = length str > 5

--4
powers :: Int -> Int -> [Int]
powers k max = takeWhile (<=max) (map (k^) [0..max])

--5
sumsOf :: [Int] -> [Int]
sumsOf [] = []
sumsOf xs = map aux [1..length xs]
    where aux x = sum (take x xs)

--6
f1 :: Int -> [Int]
f1 0 = []
f1 n = squares 1 1 n

squares :: Int -> Int -> Int -> [Int]
squares _ _ 0 = []
squares atual contador n
    |contador <= n =
        case () of
            ()  |let str = show(square atual) in head str == last str -> [square atual] ++ squares (atual+1) (contador+1) n
                |otherwise -> squares (atual+1) (contador) n
    |otherwise = []

square :: Int -> Int
square n = n*n   
 