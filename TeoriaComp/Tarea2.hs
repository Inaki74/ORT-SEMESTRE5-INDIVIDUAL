
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
    (<<) [] [] = []
    (<<) m [] = m
    (<<) [] m =  m
    (<<) m1 ((var2, val2):ms2) = findAndReplace m1 (var2, val2) << ms2

    -- 5)
    evaluacion :: Mem -> Exp -> Val
    evaluacion m (Var v) = m @@ v
    evaluacion m (Apl c et) = Val (c, map (\e -> evaluacion m e) et)

    -- 6)
    ejecutar :: Mem -> Prog -> Mem
    -- Regla de Ejecucion para Asignacion
    ejecutar m (vt := et) = m << crearMemoria m vt (map (\e -> evaluacion m e) et) -- (m <- (x,v)techo)
    -- Regla de Ejecucion para Secuencia
    ejecutar m (p1 :> p2) = ejecutar (ejecutar m p1) p2
    -- Regla de Ejecucion para Case
    ejecutar m (Case x rt) = ejecutar (m << crearMemoria m (primPar(buscarEnRamas (primParValor(evaluacion m x)) rt)) (segParValor(evaluacion m x))) (segPar(buscarEnRamas (primParValor(evaluacion m x)) rt))
    -- Regla 1 de Ejecucion para While
    -- Regla 2 de Ejecucion para While


    -- 7)
    notImp :: Prog
    notImp = Case (Var "b") [
                ("True",([], ["b"] := [Apl "False" []])),
                ("False",([], ["b"] := [Apl "True" []]))
            ]


    -- FUNCIONES AUXILIARES
    findAndReplace :: Mem -> (Var, Val) -> Mem
    findAndReplace [] v = [v]
    findAndReplace ((var1, val1):ms) (var2, val2)
                                |   var1 == var2 = (var1, val2):ms
                                |   otherwise = (var1, val1):findAndReplace ms (var2, val2)

    crearMemoria :: Mem -> [Var] -> [Val] -> Mem
    crearMemoria m [] [] = []
    crearMemoria m (var:vart) [] = (var, Null):crearMemoria m vart []
    crearMemoria m [] et = []
    crearMemoria m (var:vart) (e:et) = (var, e):crearMemoria m vart et

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

    primPar :: (a, b) -> a
    primPar (a,b) = a

    segPar :: (a, b) -> b
    segPar (a,b) = b

    -- TESTS
    --memoria
    memoria1 :: Mem
    memoria1 = [("x", Val("O",[])), ("y", Val("S", [Val ("O", [])]))]
    
    memoria2 :: Mem
    memoria2 = [("a", Val("Empty",[])), ("b", Val("List", [Val ("O", []), Val ("Empty", [])]))]

    memoria3 :: Mem
    memoria3 = [("y", Val ("O", [])), ("a", Val ("O", []))]

    --expresiones

    expresion1 :: Exp
    expresion1 = Var "x"

    expresion2 :: Exp
    expresion2 = Var "b"

    expresion3 :: Exp
    expresion3 = Var "d"

    expresion4 :: Exp
    expresion4 = Apl "Empty" []

    expresion5 :: Exp
    expresion5 = Apl "List" [Var "x", Apl "Empty" []]

    expresion6 :: Exp
    expresion6 = Apl "List" [Var "b", Apl "List" [Var "x", Apl "List" [Var "a" , Apl "Empty" []]]]

    -- funciones en memoria
    pruebaLkup1 :: Val
    pruebaLkup1 = memoria1 @@ "x" -- Val("O", [])

    pruebaLkup2 :: Val
    pruebaLkup2 = memoria1 @@ "a" -- Null

    pruebaLkup3 :: Val
    pruebaLkup3 = memoria2 @@ "x" -- Null

    pruebaLkup4 :: Val
    pruebaLkup4 = memoria2 @@ "b" -- Val("List", [Val ("O", []), Val ("Empty", [])])

    pruebaUpdate1 :: Mem
    pruebaUpdate1 = memoria1 << memoria2 -- Un orden de esto: [("x", Val("O",[])), ("y", Val("S", [Val ("O", [])]),("a", Val("Empty",[])), ("b", Val("List", [Val ("O", []), Val ("Empty", [])]))]

    pruebaUpdate2 :: Mem
    pruebaUpdate2 = memoria1 << memoria3 -- Un orden de esto: [("x", Val("O",[])), ("y", Val ("O", [])), ("a", Val ("O", []))]
    
    pruebaUpdate3 :: Mem
    pruebaUpdate3 = memoria3 << memoria1 -- Un orden de esto: [("x", Val("O",[])), ("y", Val("S", [Val ("O", [])])), ("a", Val ("O", []))]

    -- evaluaciones
    pruebaEval1 :: Val
    pruebaEval1 = evaluacion memoria1 expresion1 -- Val("O", [])

    pruebaEval2 :: Val
    pruebaEval2 = evaluacion memoria1 expresion2 -- "Null"

    pruebaEval3 :: Val
    pruebaEval3 = evaluacion memoria1 expresion1 -- "O"

    pruebaEval4 :: Val
    pruebaEval4 = evaluacion memoria1 expresion5 -- Val ("List", [Val ("O",[]), Val "Empty" []])

    pruebaEval5 :: Val
    pruebaEval5 = evaluacion memoria2 expresion6 -- Val "List" [Val("List", [Val ("O", []), Val ("Empty", [])]), Val "List" [Null, Val "List" [Val("Empty",[]) , Val "Empty" []]]]

    -- Programas
    asignacion1 :: Prog
    asignacion1 = ["b"] := [Apl "True" []]

    asignacion2 :: Prog
    asignacion2 = ["a"] := [Apl "List" [Apl "O" [], Apl "Empty" []]]

    asignacion3 :: Prog
    asignacion3 = ["b"] := [Apl "O" []]

    asignacion4 :: Prog
    asignacion4 = ["b", "e", "j"] := [Apl "S" [], Apl "O" [], Apl "A" []]

    secuencia1 :: Prog
    secuencia1 = asignacion1 :> asignacion2

    secuencia2 :: Prog
    secuencia2 = asignacion3 :> asignacion1

    case1 :: Prog
    case1 = Case (Var "y") [
        ("O", ([], asignacion1)),
        ("S", (["i1"], ["x"] := [Var "i1"]))
        ]

    -- Ejecuciones
    pruebaEjec1 :: Mem
    pruebaEjec1 = ejecutar [] asignacion1 -- [("b", Val ("True", []))]

    pruebaEjec2 :: Mem
    pruebaEjec2 = ejecutar [] secuencia1 -- [("b", Val ("True", [])), ("a",Val "List" [Val "O" [], Val "Empty" []])]

    pruebaEjec3 :: Mem
    pruebaEjec3 = ejecutar [] secuencia2 -- [("b", Val ("True", []))]

    pruebaEjecNotImp1 :: Mem
    pruebaEjecNotImp1 = ejecutar [] notImp -- error

    pruebaEjecNotImp2 :: Mem
    pruebaEjecNotImp2 = ejecutar [("a", Val ("O", []))] notImp -- error

    pruebaEjecNotImp3 :: Mem
    pruebaEjecNotImp3 = ejecutar [("b", Val ("O", []))] notImp -- error

    pruebaEjecNotImp4 :: Mem
    pruebaEjecNotImp4 = ejecutar [("b", Val ("True", []))] notImp -- [("b", Val ("False", []))]

    pruebaEjecNotImp5 :: Mem
    pruebaEjecNotImp5 = ejecutar [("b", Val ("False", []))] notImp -- [("b", Val ("True", []))]