main = interact $ show . nat2Int . read

data Nat = Zero | Succ Nat
    deriving (Eq,Show,Read)

nat2Int :: Nat -> Int
nat2Int Zero = 0
nat2Int (Succ x) = 1 + nat2Int x