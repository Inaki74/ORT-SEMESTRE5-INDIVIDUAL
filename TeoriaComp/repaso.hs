
module Repaso where
    -- Parte 0: Yo
    sucesor :: N -> N
    sucesor = S

    par :: N -> Bool
    par O = True
    par (S n) = not (par n)

    suma :: Int -> Int -> Int
    suma a b = a + b

    sumatoria :: [Int] -> Int
    sumatoria = foldr suma 0 -- Cosas raras del linter

    -- Parte 1: Data y Recursion
    data N = O | S N
        deriving (Show, Eq)

    howMany :: (Integer -> Bool) -> Integer -> Integer -> Integer
    howMany p a b 
                | a > b = 0
                | a <= b && p a = 1 + howMany p (a + 1) b
                | a <= b && not(p a) = howMany p (a + 1) b

    divides :: Integer -> Integer -> Bool
    divides a b = mod a b == 0

    prime :: Integer -> Bool
    prime 1 = True 
    prime n = howMany (\x -> divides n x) 1 n == 2

    -- Parte 2: Listasexp4 * exp3
    insertSort :: Ord a => [a] -> [a]
    insertSort [] = []
    insertSort (l:ls) = insert l (insertSort ls)

    insert :: Ord a => a -> [a] -> [a]
    insert n [] = [n]
    insert n (l:ls) 
            | n > l = l:(insert n ls)
            | n <= l = n:l:ls

    myLookUp :: Eq a => [(a,b)] -> a -> b -- Lookup mio hecho intuitivo
    myLookUp [] a = error "That element isnt present"
    myLookUp ((la, lb):ls) a 
                    | la == a = lb
                    | otherwise = myLookUp ls a

    hisLookUp :: Eq a => a -> [(a,b)] -> Maybe b -- El lookup de Alvaro
    hisLookUp _ [] = Nothing 
    hisLookUp a ((la, lb):ls)
                    | a == la = Just lb
                    | otherwise = hisLookUp a ls

    -- Parte 3: Arboles
    {-data Exp = L Integer 
            | (:+) Exp Exp
            | (:*) Exp Exp
        deriving Show

    ejecutarExp :: Exp -> Integer 
    ejecutarExp (L i) = i
    ejecutarExp (e1 :+ e2) = ejecutarExp e1 + ejecutarExp e2
    ejecutarExp  (e1 :* e2) = ejecutarExp e1 * ejecutarExp e2-}

    -- Parte 3.1: wtf
    data Exp = V String -- Define el tipo arbol
            | L Integer 
            | (:+) Exp Exp
            | (:*) Exp Exp
        deriving Show

    type Env = [(String, Integer)]  -- Le da nombre a algo ya existente
    eval :: Env -> Exp -> Integer
    eval e (V i) = myLookUp e i
    eval e (ex1 :+ ex2) = eval e ex1 + eval e ex2
    eval e (ex1 :* ex2) = eval e ex1 * eval e ex2