import Data.List

data Prop = Const Bool | Var Char | Not Prop
    | And (Prop) (Prop) | Or (Prop) (Prop)
    | Implies (Prop) (Prop) | Iff (Prop) (Prop)
        deriving (Eq, Show, Read)

--tipo Subst (no retrieveVal, caso a variavel nao seja encontrada, retorna falso)
type Atrib = (Char, Bool)
type Subst = [Atrib]

retrieveVal :: Subst -> Char -> Bool
retrieveVal [] _ = False
retrieveVal ((x, boolean):xs) query
    |x == query = boolean
    |otherwise = retrieveVal xs query

--funcao eval
eval :: Subst -> Prop -> Bool
eval _ (Const boolean) = boolean
eval table (Var x) = retrieveVal table x
eval table (Not prop) = not (eval table prop)
eval table (And prop1 prop2) = (eval table prop1) && (eval table prop2)
eval table (Or prop1 prop2) = (eval table prop1) || (eval table prop2)
eval table (Implies prop1 prop2) = implies (eval table prop1) (eval table prop2)
eval table (Iff prop1 prop2) = ifOnlyIf (eval table prop1) (eval table prop2)

--funcoes de auxilio/tabela verdade
implies :: Bool -> Bool -> Bool
implies True True = True
implies True False = False
implies False True = True
implies False False = True

ifOnlyIf :: Bool -> Bool -> Bool
ifOnlyIf True True = True
ifOnlyIf True False = False
ifOnlyIf False True = False
ifOnlyIf False False = True

--funcao vars
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not prop) = vars prop
vars (And prop1 prop2) = vars prop1 ++ vars prop2
vars (Or prop1 prop2) = vars prop1 ++ vars prop2
vars (Implies prop1 prop2) = vars prop1 ++ vars prop2
vars (Iff prop1 prop2) = vars prop1 ++ vars prop2

--funcao bools
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools 1 = [[True], [False]]
bools x = (boolsAux True (bools (x-1))) ++ (boolsAux False (bools (x-1)))

boolsAux :: Bool -> [[Bool]] -> [[Bool]]
boolsAux _ [] = []
boolsAux b (lista:listas) = ([b] ++ lista) : (boolsAux b listas)

--funcao substs
substs :: Prop -> [Subst]
substs prop = makeSubsts variaveis tabelaVerdade
    where
        variaveis = nub (vars prop)
        tabelaVerdade = bools (length (variaveis))

makeSubsts ::  [Char] -> [[Bool]] -> [Subst]
makeSubsts [] _ = []
makeSubsts _ []= []
makeSubsts cs (lBool:lBools) = [makeSubst cs lBool] ++ makeSubsts cs lBools

makeSubst :: [Char] -> [Bool] -> Subst
makeSubst [] _ = []
makeSubst _ []= []
makeSubst (c:cs) (bool:bools) = [(c, bool)] ++ makeSubst cs bools

--funcao isTaut
isTaut :: Prop -> Bool
isTaut prop = isTautHelp (substs prop) prop

isTautHelp :: [Subst] -> Prop -> Bool
isTautHelp [] _ = True
isTautHelp (s:ss) prop = eval s prop && isTautHelp ss prop
