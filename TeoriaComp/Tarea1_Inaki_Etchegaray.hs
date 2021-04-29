------------------------------
-- Iñaki Etchegaray
-- 240172
------------------------------

module Tarea1 where

    -- 1) Declarar data que represente la sintaxis de Chi.
    -- e ::= x | C | \xtecho e | e etecho | case e rtecho | rec e
    -- r ::= C xtecho e
    type Id = String

    type Rama = (Id, ([Id], Expr))  -- La Rama

    data Expr =
            Var Id              -- x
        |   C Id                -- C
        |   Lam [Id] Expr       -- Expresion Lambda
        |   Apl Expr [Expr]     -- Expresion Aplicacion
        |   Case Expr [Rama]    -- Expresion Case
        |   Rec Expr            -- Expresion Recursiva
        deriving Show

    -- 2) Definir un predicado que caracterice valores debiles.
    -- Un valor es una expresion que no es evaluable, ya terminamos de evaluarla.
    -- Son nombres nomas, strings.
    -- Un valor debil es 
    -- Ejemplos de valores debiles:
    -- 
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
                    | otherwise = Var i

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
    reducir (Var i) = error ("Variable libre." ++ i)
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

    -- Caso Apl Ci []
    -- ->
    -- 6)
    -- En la evaluacion fuerte:
    -- Regla mas general:
    -- Para evaluar fuertemente, primero evaluamos al valor debil, luego evaluamos fuertemente ese valor debil.
    -- Es decir, el resultado de evaluar fuertemente a un e, es la evaluacion fuerte v de su evaluacion debil w.
    -- Reglas mas especificas:
    -- Los lambda y los constructores atomicos evaluan fuertemente a si mismos.
    -- Para evaluar fuertemente un constructor con expresiones, se deben evaluar completamente todas las expresiones del constructor
    evaluacionFuerte :: Expr -> Expr
    evaluacionFuerte (C i) = C i
    evaluacionFuerte (Lam vt e) = Lam vt e
    evaluacionFuerte (Apl (C i) et)
                            | null et = error "Al evaluar fuertemente, deberia de ser una lista no vacia."
                            | otherwise = Apl (C i) (map evaluacionFuerte et)
    evaluacionFuerte e = evaluacionFuerte (evaluacionDebil e)

    -- 7)
    -- venis bien, mirate las imagenes que sacaste con la consulta con Seba.
    -- los constructores estan a cargo de programador
    -- 

    --Not :: Bool -> Bool 
    --Not b = Case b of {
    --    True -> False;
    --    False -> True;
    --}
    -- NOT
    chiNot :: Expr
    chiNot = Lam ["b"] (Case (Var "b") [ 
        ("True", ([], C "False")), 
        ("False", ([], C "True"))
        ])

    --Par :: N -> Bool
    --Par n = Case n of {
    --    O -> True 
    --    S x -> not (Par x)
    --}
    -- PAR
    chiPar :: Expr
    chiPar = Rec ( Lam ["par"] (Lam ["n"] (Case (Var "n") [
        ("O", ([], C "True")), 
        ("S",(["x"], Apl chiNot [Apl (Var "par") [Var "x"]]))
        ])))

    -- Largo :: [a] -> N
    -- Largo l = Case l of{
    --    [] -> O
    --    (x:xs) -> S (Largo xs)
    -- }
    -- LARGO
    largo :: Expr
    largo = Rec (Lam ["largo"] (Lam ["lis"] (Case (Var "lis") [
        ("Empty", ([], C "O")), 
        ("List", (["x", "xs"], Apl (C "S") [Apl (Var "largo") [Var "xs"]]))
        ])))

    -- filtrar :: [a] -> (a -> bool) -> [a]
    -- filtrar l p = case l of{
    --      [] -> [];
    --      x:xs -> case p x of {
    --          True -> x:(filtrar xs p);
    --          False -> filtrar xs p;
    --      };
    -- }
    -- FILTRAR
    filtrar :: Expr
    filtrar = Rec (Lam ["filtrar"] (Lam ["lis"] (Lam ["pred"] (Case (Var "lis") [
          ("Empty", ([], C "Empty")),
          ("List" , (["x", "xs"], Case (Apl (Var "pred") [Var "x"]) [
                      ("True", ([], Apl (C "List") [Var "x", Apl (Var "filtrar") [Var "xs", Var "pred"]])),
                      ("False", ([], Apl (Var "filtrar") [Var "xs", Var "pred"]))
          ]))
     ]))))

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