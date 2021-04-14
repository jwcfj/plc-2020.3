main = interact $ show . perfeitos . read

perfeitos :: Int -> [Int]
perfeitos 0 = []
perfeitos n = filter isPerfeito [n, (n-1)..1]

isPerfeito :: Int -> Bool
isPerfeito x = x == (foldr (+) 0 (map quadrado (fatores x)))

fatores :: Int -> [Int]
fatores 1 = [1]
fatores 2 = [2]
fatores n = filter check [2..n-1] --o segredo tá aqui
    where check y = (n `mod` y) == 0

quadrado :: Int -> Int
quadrado n = n * n