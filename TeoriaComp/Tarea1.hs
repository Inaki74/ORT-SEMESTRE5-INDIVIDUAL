------------------------------
-- Iñaki Etchegaray
-- 240172
------------------------------

module Tarea1 where

    -- 1)
    type Id = String

    type Rama = (Id, ([Id], Expr))

    data Expr =
            Var Id              -- x
        |   C Id                -- C
        |   Lam [Id] Expr       -- Expresion Lambda
        |   Apl Expr [Expr]     -- Expresion Aplicacion
        |   Case Expr [Rama]    -- Expresion Case
        |   Rec Expr            -- Expresion Recursiva
        deriving Show

    -- 2) Definir un predicado que caracterice valores debiles.
    esValorDebil :: Expr -> Bool
    esValorDebil (C i) = True
    esValorDebil (Apl (C i) []) = False
    esValorDebil (Apl (C i) et) = True
    esValorDebil (Lam vt e) = True
    esValorDebil e = False

    -- 3) 
    -- La sustitucion se puede ver como una lista ordenada de ordenes de que variable sustituir en donde
    -- Defino una sustitucion singular:
    type Sust = (Id, Expr)
    -- Donde se puede leer como (i, e) = i := e
    -- Luego Sigma seria la sustitucion multiple, es decir, la lista de Sustituciones Singulares.
    type Sigma = [Sust]

    -- lkup busca la expresion en sigma asociada a i, sino devuelve i
    lkup :: Sigma -> Id -> Expr
    lkup [] i = Var i 
    lkup ((is,e):sig) i
                    | is == i = e
                    | otherwise = lkup sig i

    -- El menos de sustitucion
    (-.) :: Sigma -> [Id] -> Sigma
    (-.) [] vt = []
    (-.) s [] = s
    (-.) s (v:vt) = findAndRemoveId s v -. vt

    sustitucion :: Expr -> Sigma -> Expr
    sustitucion (Var i) s = lkup s i
    sustitucion (C i) s = C i
    sustitucion (Lam vt e) s = Lam vt (sustitucion e (s -. vt))
    sustitucion (Apl e et) s = Apl (sustitucion e s)(map (`sustitucion` s) et)
    sustitucion (Case e rt) s = Case (sustitucion e s)(map (`sustitucionRama` s) rt)
    sustitucion (Rec e) s = Rec (sustitucion e s)

    sustitucionRama :: Rama -> Sigma -> Rama
    sustitucionRama (i, (xt,e)) s = (i, (xt, sustitucion e (s -. xt)))

    -- 4)
    reducir :: Expr -> Expr
    reducir (Var i) = error "Variable libre."
    reducir (C i) = C i
    reducir (Lam vt e) = Lam vt e
    -- REGLA BETA
    reducir (Apl (Lam vt e) et)
                        | length vt == length et = sustitucion e (crearSigma vt et)
                        | otherwise = error "(Reducir Beta): Cantidad de variables a sustituir no es igual a la cantidad de variables ligadas."
    -- REGLA GAMMA
    reducir (Apl (Apl (C i) et1) et2) = Apl (C i) (et1 ++ et2)
    -- REGLA MU
    reducir (Apl e et) = Apl (reducir e) et
    -- REGLA EPSILON 0
    reducir (Case (C i) rt)
                        | not(estaEnRamas i rt) = error "(Reducir Epsilon 0): Case mal escrito: No se encuentra el constructor relacionado."
                        | otherwise = obtenerExpresionEnRama i rt
    --REGLA EPSILON I
    reducir (Case (Apl (C i) et) rt)
                        | not(estaEnRamas i rt) = error "(Reducir Epsilon 1): Case mal escrito: No se encuentra el constructor relacionado."
                        | length(obtenerLista i rt) == length et = sustitucion (obtenerExpresionEnRama i rt) (crearSigma (obtenerLista i rt) et)
                        | otherwise = error "(Reducir Epsilon 1): Cantidad de variables a sustituir no es igual a la cantidad de variables ligadas."
    -- REGLA DELTA
    reducir (Case e rt) = Case (reducir e) rt
    reducir (Rec e) = Apl e [Rec e]

    -- 5)
    evaluacionDebil :: Expr -> Expr
    evaluacionDebil e
                    | esValorDebil e = e
                    | otherwise = evaluacionDebil (reducir e)

    -- 6)
    evaluacionFuerte :: Expr -> Expr
    evaluacionFuerte (C i) = C i
    evaluacionFuerte (Lam vt e) = Lam vt e
    evaluacionFuerte (Apl (C i) et)
                            | null et = error "Al evaluar fuertemente, deberia de ser una lista no vacia."
                            | otherwise = Apl (C i) (map evaluacionFuerte et)
    evaluacionFuerte e = evaluacionFuerte (evaluacionDebil e)

    -- 7)
    -- FUNCIONES
    chiNot :: Expr
    chiNot = Lam ["b"] (Case (Var "b") [ 
        ("True", ([], C "False")), 
        ("False", ([], C "True"))
        ])

    chiPar :: Expr
    chiPar = Rec ( Lam ["par"] (Lam ["n"] (Case (Var "n") [
        ("O", ([], C "True")), 
        ("S",(["x"], Apl chiNot [Apl (Var "par") [Var "x"]]))
        ])))

    chiLargo :: Expr
    chiLargo = Rec (Lam ["largo"] (Lam ["lis"] (Case (Var "lis") [
        ("Empty", ([], C "O")), 
        ("List", (["x", "xs"], Apl (C "S") [Apl (Var "largo") [Var "xs"]]))
        ])))

    chiFiltrar :: Expr
    chiFiltrar = Rec (Lam ["filtrar"] (Lam ["lis", "pred"] (Case (Var "lis") [
          ("Empty", ([], C "Empty")),
          ("List" , (["x", "xs"], Case (Apl (Var "pred") [Var "x"]) [
                      ("True", ([], Apl (C "List") [Var "x", Apl (Var "filtrar") [Var "xs", Var "pred"]])),
                      ("False", ([], Apl (Var "filtrar") [Var "xs", Var "pred"]))
          ]))
     ])))

    -- PRUEBAS
    largoListaCeroUnoDosYTres :: Expr
    largoListaCeroUnoDosYTres = evaluacionFuerte (Apl chiLargo [listaConCeroUnoDosYTres])
    -- Resultado esperado: Apl (C "S") [Apl (C "S") [Apl (C "S") [Apl (C "S") [C "O"]]]]

    filtrarParesListaCeroUnoDosYTres :: Expr
    filtrarParesListaCeroUnoDosYTres = evaluacionFuerte (Apl chiFiltrar [listaConCeroUnoDosYTres, chiPar])
    -- Resultado esperado: Apl (C "List") [C "O" , Apl (C "List") [Apl (C "S") [Apl (C "S") [C "O"]], C "Empty"]]
    
    -- PRUEBAS EXTRA
    largoListaCeroUnoDosYTresFiltradaPares :: Expr
    largoListaCeroUnoDosYTresFiltradaPares = evaluacionFuerte (Apl chiLargo [filtrarParesListaCeroUnoDosYTres])
    -- Resultado esperado: Apl (C "S") [Apl (C "S") [C "O"]]

    filtrarNotListaTruesYFalse :: Expr
    filtrarNotListaTruesYFalse = evaluacionFuerte (Apl chiFiltrar [listaConTruesYFalse, chiNot]);
    -- Resultado esperado: Apl (C "List") [C "False",C "Empty"]

    -----------------------------------------------
    -----------------------------------------------
    -- VALORES
    cero :: Expr
    cero = C "O"

    uno :: Expr
    uno = Apl (C "S") [cero]

    dos :: Expr
    dos = Apl (C "S") [uno]

    tres :: Expr
    tres = Apl (C "S") [dos]

    listaConCeroUnoDosYTres :: Expr
    listaConCeroUnoDosYTres = Apl (C "List") [cero, 
                                    Apl (C "List") [uno, 
                                            Apl (C "List") [dos, 
                                                    Apl (C "List") [tres, C "Empty"]]]]

    listaConTruesYFalse :: Expr
    listaConTruesYFalse = Apl (C "List") [C "True", 
                                    Apl (C "List") [C "True", 
                                            Apl (C "List") [C "False", 
                                                    Apl (C "List") [C "True", C "Empty"]]]]

    -- FUNC AUXILIARES
    findAndRemoveId :: Sigma -> Id -> Sigma
    findAndRemoveId [] i2 = []
    findAndRemoveId ((i1,e):sig) i2
                            | i1 == i2 = sig
                            | otherwise = (i1,e):findAndRemoveId sig i2

    -- Pre: El largo de [Id] y [Exp] debe ser igual. Se cumple por ser utlizado solo 
    -- en la regla Beta.
    crearSigma :: [Id] -> [Expr] -> Sigma
    crearSigma [] [] = []
    crearSigma (i:is) (e:es) = (i,e):crearSigma is es

    -- No quise usar lookup porque me incomoda el Maybe
    estaEnRamas :: Id -> [Rama] -> Bool
    estaEnRamas i [] = False
    estaEnRamas i ((id,resto):rt)
                            | i == id = True
                            | otherwise = estaEnRamas i rt

    obtenerLista :: Id -> [Rama] -> [Id]
    obtenerLista  i [] = []
    obtenerLista  i ((id,(l,et)):rt)
                                | i == id  = l
                                | otherwise = obtenerLista i rt

    --PRE: Se encuentra el Id en la lista de ramas.
    obtenerExpresionEnRama :: Id -> [Rama] -> Expr
    obtenerExpresionEnRama i ((id,(v,e)):rt)
                                    | i == id = e
                                    | otherwise = obtenerExpresionEnRama i rt