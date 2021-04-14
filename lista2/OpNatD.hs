parseInput str = let (m:n:_) = lines str in (read m, read n)

main = interact $ show . uncurry mult . parseInput

data Nat = Zero | Succ Nat
    deriving (Eq,Show,Read)

mult :: Nat -> Nat -> Nat
mult x y = int2Nat ((nat2Int x) * (nat2Int y))

nat2Int :: Nat -> Int
nat2Int Zero = 0
nat2Int (Succ x) = 1 + nat2Int x

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat n = Succ (int2Nat (n-1))