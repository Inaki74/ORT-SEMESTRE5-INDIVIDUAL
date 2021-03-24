
{-# LANGUAGE TupleSections #-}
module Tarea0 where
    -- e ::= x | l | x := e | e # e
    data Exp = V String -- Variable             -- e ::= x
            | L Integer -- Literal              -- l
            | (:=) String Exp -- Asignacion -- x := e
            | (:+) Exp Exp                      -- El resto: e # e
            | (:*) Exp Exp
            | (:/) Exp Exp
            | (:%) Exp Exp
        deriving Show

    type Memoria = [(String, Integer)] -- M
    env :: Memoria
    env = []

    -- EJERCICIO 1
    -- Si importa.
    -- Esto se debe a la interaccion entre las reglas ass y bin.
    -- Si nosotros afectamos la memoria y la utilizamos en las operaciones, nos puede cambiar el resultado.
    -- Por ejemplo:
    -- ex1 + ex2
    -- (x := 5) + (x) dado M = [(x, 10)]
    -- Opcion 1: Evaluamos ex1 primero
    -- Nos da ([(x,5)], 5 + 5) = ([(x,5)], 10)
    -- Al evaluar ex2, aplica la version presente de la memoria en el momento, que es 5 ya que se evaluo ex1 primero.
    -- Opcion 2: Evaluamos ex2 primero
    -- Nos da ([(x,5)], 10 + 5) = ([(x,5)], 15)
    -- Al evaluar ex2, aplica la version presente de la memoria en el momento, que es 10 ya que se evaluo ex1 no se evaluo todavia.

    -- EJERCICIO 2
    -- PRE: x existe en m inicializada
    -- myRead = M x
    myRead :: Memoria -> String -> Integer -- Sin maybe
    myRead [] a = error "El elemento no se encuentra en la memoria"
    myRead ((la, lb):ls) a
                    | la == a = lb
                    | otherwise = myRead ls a

    -- write M x i = M<+(x,i)
    write :: Memoria -> String -> Integer -> Memoria
    write [] x v = [(x,v)]
    write m x v
                | findEnMemoria m x = actualizarMemoria m x v -- Si esta en la memoria, actualizar
                | otherwise = (x,v):m -- Si no esta, simplemente agregar

    -- EJERCICIO 3
    eval :: Memoria -> Exp -> (Memoria, Integer)
    -- M, L ↓ M, L : Regla Lit
    eval m (L i) = (m, i)
    -- M, x ↓ M, M x : regla Var
    eval m (V i) = (m, myRead m i)
    -- M, x := e ↓ M'<+(x, n), n : regla Ass.
    -- eval m ex = (M', n) => primPar(eval m ex) = M' y segPar(eval m ex) = n, el eval m ex es M, e ↓ M', n aca
    -- Y como write M x i = M<+(x,i) =>  
    --    (write (primPar(eval m ex) x (segPar(eval m ex)), segPar(eval m ex)) = (M'<+(x,n), n)
    eval m (x := ex) = (write (primPar(eval m ex)) x (segPar(eval m ex)), segPar(eval m ex))
    -- M, e1 O e2 ↓ M'', m # n : regla Bin
    -- En cada caso de operacion matematica, para mantener las premisas de la regla: M, e1 ↓ M', m | M', e2 ↓ M'', n,
    -- debemos conseguir la memoria de evaluar la ex2 luego de evaluar la ex1. Luego esta el numero resultado de sumar con el m original.
    -- El resultado es (M'', m O n) que es el resultado de la regla bin.
    -- SUMA
    eval m (ex1 :+ ex2) = (primPar(eval (primPar(eval m ex1)) ex2), segPar(eval m ex1) + segPar(eval m ex2))
    -- PRODUCTO
    eval m (ex1 :* ex2) = (primPar(eval (primPar(eval m ex1)) ex2), segPar(eval m ex1) * segPar(eval m ex2))
    -- DIVISION
    eval m (ex1 :/ ex2) = (primPar(eval (primPar(eval m ex1)) ex2), div (segPar(eval m ex1)) (segPar(eval m ex2)))
    -- MODULO
    eval m (ex1 :% ex2) = (primPar(eval (primPar(eval m ex1)) ex2), mod (segPar(eval m ex1)) (segPar(eval m ex2)))



    -- FUNCIONES AUXILIARES
    findEnMemoria :: Memoria -> String -> Bool
    findEnMemoria [] x = False
    findEnMemoria ((mx, mv):ms) x
                    | mx == x = True
                    | mx /= x = findEnMemoria ms x

    -- PRE: x existe en m inicializada
    actualizarMemoria :: Memoria -> String -> Integer -> Memoria
    actualizarMemoria [] x v = error "El elemento no se encuentra en la memoria"
    actualizarMemoria ((mx, mv):ms) x v
                    |  mx == x = (mx, v):ms
                    | otherwise =  (mx, mv):actualizarMemoria ms x v

    primPar :: (a, b) -> a
    primPar (a,b) = a

    segPar :: (a, b) -> b
    segPar (a,b) = b