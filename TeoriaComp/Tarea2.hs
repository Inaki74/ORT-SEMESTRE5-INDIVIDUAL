
-- Iñaki Etchegaray
-- 240172
------------------------------

module Tarea2 where
    -- 1)
    type Var = String

    data Exp =
            Var Var     -- Variable x
        |   Apl C [Exp]    -- Constructor aplicado
        deriving Show

    -- 2)
    data Prog =
            (:>) Prog Prog      -- Secuencia
        |   (:=) [Var] [Exp]    -- Asignacion
        |   Case Var [Rama]     -- Seleccion
        |   While Var [Rama]    -- Iteracion
        deriving Show

    type C = String

    type Rama = (C, ([Var], Prog))

    -- 3)
    data Val = 
            Val (C ,[Val])     -- Valor
        |   Null
        deriving Show

    -- 4)
    type Mem = [(Var, Val)]

    -- Lookup
    (@@) :: Mem -> Var -> Val
    (@@) [] v1 = Null
    (@@) ((v2,val):m) v1  
                    |   v1 == v2 = val
                    |   otherwise = m@@v1

    -- Update
    (<<) :: Mem -> Mem -> Mem
    (<<) m p = m++p

    -- 5)
    evaluacion :: Mem -> Exp -> Val
    evaluacion m (Var v) = m @@ v
    evaluacion m (Apl c et) = Val (c, map (\e -> evaluacion m e) et)

    -- 6)
    ejecutar :: Mem -> Prog -> Mem
    -- Regla de Ejecucion para Asignacion
    ejecutar m (vt := et) = m << crearMemoria m vt et
    -- Regla de Ejecucion para Secuencia
    ejecutar m (p1 :> p2) = ejecutar (ejecutar m p1) p2
    -- Regla de Ejecucion para Case
    -- Regla 1 de Ejecucion para While
    -- Regla 2 de Ejecucion para While

    -- FUNCIONES AUXILIARES
    crearMemoria :: Mem -> [Var] -> [Exp] -> Mem
    crearMemoria m [] [] = []
    crearMemoria m (var:vart) [] = (var, Null):crearMemoria m vart []
    crearMemoria m [] et = []
    crearMemoria m (var:vart) (e:et) = (var, evaluacion m e):crearMemoria m vart et

    --buscarEnRamas :: C -> [Rama] -> ([Var], Prog)
    --buscarEnRamas c rt = (,)

    -- TESTS
    pruebaMem1 :: Mem
    pruebaMem1 = [("x", Val("O",[])), ("y", Val("S", [Val ("O", [])]))]
    
    pruebaMem2 :: Mem
    pruebaMem2 = [("a", Val("Empty",[])), ("b", Val("List", [Val ("O", []), Val ("Empty", [])]))]

    pruebaExp1 :: Exp
    pruebaExp1 = Var "x"

    pruebaExp2 :: Exp
    pruebaExp2 = Var "b"

    pruebaExp3 :: Exp
    pruebaExp3 = Var "d"

    pruebaExp4 :: Exp
    pruebaExp4 = Apl "Empty" []

    pruebaExp5 :: Exp
    pruebaExp5 = Apl "List" [Var "x", Apl "Empty" []]