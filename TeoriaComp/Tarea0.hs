
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
    -- Esto se debe a la evaluacion de la asignacion.
    -- Si sumamos ANTES de asignar, el valor que termina asignado en memoria va a ser distinto si sumamos LUEGO de asignar.
    -- Por ejemplo:
    -- Dado M = [("x", 4), ("y", 3)]
    -- Y lo siguiente: (x := y) :+ (y := y + 2)
    -- Si sumamos ANTES de asignar el siguiente es el resultado:
    -- ([("x", 5), ("y", 3)], 5)
    -- Si sumamos LUEGO de asignar el siguiente es el resultado:
    -- ([("x")], 5)

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
    -- debemos definir los casos de asignacion antes y manejarlos. Debido al orden de definicion puesto aca, 
    -- siempre se va a hacer la asignacion primero y luego la operacion. El resultado final, por lo tanto, va a ser en M'',
    -- como es definido en la regla.
    -- SUMA
    eval m ((x := ex1) :+ ex2) = eval (write (primPar(eval m ex1)) x (segPar(eval m ex1))) (ex1 :+ ex2)
    eval m (ex1 :+ (x := ex2)) = eval (write (primPar(eval m ex2)) x (segPar(eval m ex2))) (ex1 :+ ex2)
    eval m (ex1 :+ ex2) = (m,segPar(eval m ex1) + segPar(eval m ex2))
    -- PRODUCTO
    eval m ((x := ex1) :* ex2) = eval (write (primPar(eval m ex1)) x (segPar(eval m ex1))) (ex1 :* ex2)
    eval m (ex1 :* (x := ex2)) = eval (write (primPar(eval m ex2)) x (segPar(eval m ex2))) (ex1 :* ex2)
    eval m (ex1 :* ex2) = (m,segPar(eval m ex1) * segPar(eval m ex2))
    -- DIVISION
    eval m ((x := ex1) :/ ex2) = eval (write (primPar(eval m ex1)) x (segPar(eval m ex1))) (ex1 :/ ex2)
    eval m (ex1 :/ (x := ex2)) = eval (write (primPar(eval m ex2)) x (segPar(eval m ex2))) (ex1 :/ ex2)
    eval m (ex1 :/ ex2) = (m, div (segPar(eval m ex1)) (segPar(eval m ex2)))
    -- MODULO
    eval m ((x := ex1) :% ex2) = eval (write (primPar(eval m ex1)) x (segPar(eval m ex1))) (ex1 :% ex2)
    eval m (ex1 :% (x := ex2)) = eval (write (primPar(eval m ex2)) x (segPar(eval m ex2))) (ex1 :% ex2)
    eval m (ex1 :% ex2) = (m, mod (segPar(eval m ex1)) (segPar(eval m ex2)))



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
                    | otherwise = actualizarMemoria ms x v

    primPar :: (a, b) -> a
    primPar (a,b) = a

    segPar :: (a, b) -> b
    segPar (a,b) = b