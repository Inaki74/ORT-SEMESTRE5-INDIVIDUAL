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

    -- 2) Definir un predicado que caracterice valores debiles.
    -- Un valor es una expresion que no es evaluable, ya terminamos de evaluarla.
    -- Son nombres nomas, strings.
    -- Un valor debil es 
    -- Ejemplos de valores debiles:
    -- 
    esValorDebil :: Expr -> Bool 
    esValorDebil (C i) = True
    esValorDebil (Apl (C etc) et) = True --TODO
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

    --TODO
    (-.) :: Sigma -> [Id] -> Sigma
    (-.) [] vt = []
    (-.) (s:sigmap) vt = []

    sustitucion :: Expr -> Sigma -> Expr
    sustitucion (Var i) s = lkup s i
    sustitucion (C i) s = C i
    sustitucion (Lam vt e) s = Lam vt (sustitucion e (s -. vt))
    sustitucion (Apl e et) s = Apl (sustitucion e s)(map (\e -> sustitucion e s) et)
    sustitucion (Case e rt) s = Case (sustitucion e s)(map (\r -> sustitucionRama r s) rt)
    sustitucion (Rec e) s = Rec (sustitucion e s)

    sustitucionRama :: Rama -> Sigma -> Rama
    sustitucionRama (i, (xt,e)) s = (i, (xt, sustitucion e (s -. xt)))


    -- FUNC AUXILIARES
    -- Hice mi propio lookup porque Maybe me queda muy incomodo