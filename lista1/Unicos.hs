import Data.List
main = do
  x <- getLine
  print $ unicos (map (read :: String -> Int) (words x))

unicos :: [Int] -> [Int]
unicos xs = aux (freqs xs)

aux :: Eq a => [(Int, a)] -> [a]
aux [] = []
aux ((x, elem):xs)
  |x == 1 = [elem] ++ aux xs
  |otherwise = aux xs

freqs :: Eq a => [a] -> [(Int, a)]
freqs [] = []
freqs (x:xs) = (contador+1, x) : freqs dif 
    where
        (iguais, dif) = partition (==x) xs
        contador = length iguais