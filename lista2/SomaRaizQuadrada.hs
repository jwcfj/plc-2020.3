main = interact $ show . somaSqrt . read

somaSqrt :: (Floating a, Ord a) => [a] -> a
somaSqrt [] = 0
somaSqrt xs = foldAux (map sqrt (filter (>0) xs))

foldAux :: Floating a => [a] -> a
foldAux xs = foldr (+) 0 xs