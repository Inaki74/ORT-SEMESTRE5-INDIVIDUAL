------------------------------
-- Iñaki Etchegaray
-- 240172
------------------------------
{-# LANGUAGE TupleSections #-}
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
    sustitucion (Apl e et) s = Apl (sustitucion e s)(map (\e -> sustitucion e s) et)
    sustitucion (Case e rt) s = Case (sustitucion e s)(map (\r -> sustitucionRama r s) rt)
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
                        | otherwise = error "Cantidad de variables a sustituir no es igual a la cantidad de variables ligadas."
    -- REGLA GAMMA
    reducir (Apl (Apl (C i) et1) et2) = Apl (C i) (et1 ++ et2)
    -- REGLA MU
    reducir (Apl e et) = Apl (reducir e) et
    -- REGLA EPSILON 0
    reducir (Case (C i) rt) 
                        | not(estaEnRamas i rt) = error "Case mal escrito: No se encuentra el constructor relacionado."
                        | otherwise = obtenerExpresionEnRama i rt
    --REGLA EPSILON I
    reducir (Case (Apl (C i) et) rt)
                        | not(estaEnRamas i rt) = error "Case mal escrito: No se encuentra el constructor relacionado."
                        | length(obtenerLista i rt) == length et = sustitucion (obtenerExpresionEnRama i rt) (crearSigma (obtenerLista i rt) et) 
                        | otherwise = error "Cantidad de variables a sustituir no es igual a la cantidad de variables ligadas."
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
    evaluacionFuerte 

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