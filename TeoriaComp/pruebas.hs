
module Pruebas where

    data N = O | S N
        deriving Show

    sucesor :: N -> N
    sucesor = S

    par :: N -> Bool
    par O = True
    par (S n) = not (par n)

    suma :: Int -> Int -> Int
    suma a b = a + b

    sumatoria :: [Int] -> Int
    sumatoria = foldr suma 0 -- Cosas raras del linter

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
    