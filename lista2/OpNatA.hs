main = interact $ show . int2Nat . read

data Nat = Zero | Succ Nat
    deriving (Eq,Show,Read)

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat n = Succ (int2Nat (n-1))