
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
        |   Case Exp [Rama]     -- Seleccion
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
    ejecutar m (vt := et) = m << crearMemoria m vt et -- (m <- (x,v)techo)
    -- Regla de Ejecucion para Secuencia
    ejecutar m (p1 :> p2) = ejecutar (ejecutar m p1) p2
    -- Regla de Ejecucion para Case
    ejecutar m (Case x rt) = ejecutar (m << crearMemoria m (segParValor(evaluacion m x)) (primParValor(buscarEnRamas (primParValor(evaluacion m x)) rt))) (segParValor(buscarEnRamas (primParValor(evaluacion m x)) rt))
    -- Regla 1 de Ejecucion para While
    -- Regla 2 de Ejecucion para While
    -- FUNCIONES AUXILIARES
    crearMemoria :: Mem -> [Var] -> [Exp] -> Mem
    crearMemoria m [] [] = []
    crearMemoria m (var:vart) [] = (var, Null):crearMemoria m vart []
    crearMemoria m [] et = []
    crearMemoria m (var:vart) (e:et) = (var, evaluacion m e):crearMemoria m vart et

    buscarEnRamas :: C -> [Rama] -> ([Var], Prog)
    buscarEnRamas c [] = error ("buscarEnRamas: No se encontro una rama por el constructor: " ++ c)
    buscarEnRamas c ((c2, dupla):rs)  
                            |   c == c2 = dupla
                            |   otherwise = buscarEnRamas c rs

    primParValor :: Val -> C
    primParValor (Val (a,b)) = a
    primParValor Null = error "Valor Null sin constructor en rama."

    segParValor :: Val -> [Val]
    segParValor (Val (a,b)) = b
    segParValor Null = error "Valor Null sin constructor en rama."

    -- TESTS
    --memoria
    pruebaMem1 :: Mem
    pruebaMem1 = [("x", Val("O",[])), ("y", Val("S", [Val ("O", [])]))]
    
    pruebaMem2 :: Mem
    pruebaMem2 = [("a", Val("Empty",[])), ("b", Val("List", [Val ("O", []), Val ("Empty", [])]))]

    --expresiones

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

    pruebaExp6 :: Exp
    pruebaExp6 = Apl "List" [Var "b", Apl "List" [Var "x", Apl "List" [Var "a" , Apl "Empty" []]]]

    -- funciones en memoria
    pruebaLkup1 :: Val
    pruebaLkup1 = pruebaMem1 @@ "x" -- Val("O", [])

    pruebaLkup2 :: Val
    pruebaLkup2 = pruebaMem1 @@ "a" -- Null

    pruebaLkup3 :: Val
    pruebaLkup3 = pruebaMem2 @@ "x" -- Null

    pruebaLkup4 :: Val
    pruebaLkup4 = pruebaMem2 @@ "b" -- Val("List", [Val ("O", []), Val ("Empty", [])])

    -- pruebaUpdate1, es un concat no vale la pena

    -- evaluaciones
    pruebaEval1 :: Val
    pruebaEval1 = evaluacion pruebaMem1 pruebaExp1 -- Val("O", [])

    pruebaEval2 :: Val
    pruebaEval2 = evaluacion pruebaMem1 pruebaExp2 -- "Null"

    pruebaEval3 :: Val
    pruebaEval3 = evaluacion pruebaMem1 pruebaExp1 -- "O"

    pruebaEval4 :: Val
    pruebaEval4 = evaluacion pruebaMem1 pruebaExp5 -- Val ("List", [Val ("O",[]), Val "Empty" []])

    pruebaEval5 :: Val
    pruebaEval5 = evaluacion pruebaMem2 pruebaExp6 -- Val "List" [Val("List", [Val ("O", []), Val ("Empty", [])]), Val "List" [Null, Val "List" [Val("Empty",[]) , Val "Empty" []]]]