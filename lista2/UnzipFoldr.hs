main = interact $ show . unzip' . (read :: String -> [(Int,Int)])

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' xs = foldr aux ([], []) xs
    where aux (a, b) (xs, ys)= (([a]++xs), ([b]++ys))