

module FirstScript where

size :: Integer
size = 12+13

--      The function to square an integer.

square :: Integer -> Integer
square n = n*n

--      The function to double an integer.
        
double :: Integer -> Integer
double n = 2*n



--      An example using double, square and size.
         
example :: Integer
example = double (size - square (2+2))

f1 :: Integer -> Integer
f1 x = square (double x)

f2 :: Integer -> Integer
f2 x = double (square x)

threeDifferent :: Integer->Integer->Integer->Bool
threeDifferent x y z = not(x == y) && not(x == z)

threeEqual :: Integer->Integer->Integer->Bool
threeEqual x y z = (x == y) && (x == z)

fourEqual:: Integer->Integer->Integer->Integer->Bool
fourEqual a b c d = threeEqual a b c && (a == d)

triArea :: Float->Float->Float->Float
triArea a b c
        | possible a b c = sqrt(s*(s-a)*(s-b)*(s-c))
        | otherwise = 0
        where
                s = (a+b+c)/2
                possible :: Float->Float->Float->Bool
                possible a b c
                        | (a <= 0) || (b<=0) || (c<=0) = False
                        | (not(ineq a b c) ) || (not(ineq b a c) ) || (not(ineq c a b) ) = False
                        | otherwise = True
                        where
                                ineq :: Float->Float->Float->Bool
                                ineq x y z
                                        | x < (y+z) = True
                                        | otherwise = False


meuMin :: Integer->Integer->Integer
meuMin x y
        | x < y = x
        |otherwise = y


minThree :: Integer->Integer->Integer->Integer
minThree x y z = meuMin x (meuMin y z)