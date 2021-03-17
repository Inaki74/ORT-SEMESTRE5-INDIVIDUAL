
module Tarea0 where
    -- e ::= x | l | x := e | e # e
    data Exp = V String -- Variable             -- e ::= x
            | L Integer -- Literal              -- l
            | (:=) String Exp -- Asignacion -- x := e
            | (:+) Exp Exp                      -- El resto: e # e
            | (:*) Exp Exp
        deriving Show

    type Memoria = [(String, Integer)] -- M

    -- PRE: x existe en m inicializada
    -- LeerMemoria = M x
    leerMemoria :: Memoria -> String -> Integer -- Sin maybe
    leerMemoria [] a = error "El elemento no se encuentra en la memoria"
    leerMemoria ((la, lb):ls) a 
                    | la == a = lb
                    | otherwise = leerMemoria ls a

    findEnMemoria :: Memoria -> String -> Bool 
    findEnMemoria [] x = False 
    findEnMemoria ((mx, mv):ms) x 
                    | mx == x = True 
                    | mx /= x = findEnMemoria ms x

    -- agregarAMemoria M x i = M<+(x,i)
    agregarAMemoria :: Memoria -> String -> Integer -> Memoria
    agregarAMemoria [] x v = [(x,v)]
    agregarAMemoria m x v 
                    | findEnMemoria m x = actualizarMemoria m x v -- Si esta en la memoria, actualizar
                    | otherwise = (x,v):m -- Si no esta, simplemente agregar

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

    
    eval :: Memoria -> Exp -> (Memoria, Integer)
    -- M, L ↓ M, L : Regla Lit
    eval m (L i) = (m, i)      
    -- M, x ↓ M, M x : regla Var                             
    eval m (V i) = (m, leerMemoria m i)  
    -- M, x := e ↓ M'<+(x, n), n : regla Ass. 
    -- eval m ex = (M', n) => primPar(eval m ex) = M' y segPar(eval m ex) = n, el eval m ex es M, e ↓ M', n aca
    -- Y como agregarAMemoria M x i = M<+(x,i) =>  
    --    (agregarAMemoria (primPar(eval m ex) x (segPar(eval m ex)), segPar(eval m ex)) = (M'<+(x,n), n)
    eval m (x := ex) = (agregarAMemoria (primPar(eval m ex) x (segPar(eval m ex)), segPar(eval m ex))
    -- Despues sigo
    --eval m (ex1 :+ ex2) = ( , )
    --eval m (ex1 :* ex2) = eval m ex1 * eval m ex2