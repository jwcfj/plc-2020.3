main = do
    line <- getLine
    print (contaBatatas line)

contaBatatas :: String -> Integer
contaBatatas line
    |last line == 's' = read (take (n - 8) line) :: Integer
    |otherwise = 1
    where 
        n = (length line)