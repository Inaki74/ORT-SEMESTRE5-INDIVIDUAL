module Tarea3 where
    -- DEFINICIONES
    type Symbol = String

    data Tape = T ([Symbol], Symbol, [Symbol])
    instance Show Tape where
        show (T (l, c, r)) =
            show (reverse l) ++ " | " ++ c ++ " | " ++ show r;

    type State = String

    data Action =
                L 
            |   R
            |   Sigma String
        deriving Show 
        
    type TableTuple = ((State, Symbol),(Action, State))
    type ControlTable = [TableTuple]

    type Configuration = (State, Tape)

    -- RESERVADOS
    blank :: Symbol
    blank = "#"

    initial :: State
    initial = "i"

    halt :: State
    halt = "h"
    -------------

    -- PASO DE TRANSICION
    step :: Configuration -> TableTuple -> Configuration
    step (s1, T ([], c, r)) ((q, s2), (L, q')) = (q', T ([], blank, c:r))   -- Generacion de blancos LEFT
    step (s1, T (l:ls, c, r)) ((q, s2), (L, q')) = (q', T (ls, l, c:r))     -- Paso LEFT
    step (s1, T (l, c, [])) ((q, s2), (R, q')) = (q', T (c:l, blank, []))   -- Generacion de blancos RIGHT
    step (s1, T (l, c, r:rs)) ((q, s2), (R, q')) = (q', T (c:l, r, rs))     -- Paso LEFT
    step (s1, T (l, c, r)) ((q, s2), (Sigma k, q')) = (q', T (l, k, r))     -- Paso SIGMA

    -- EVALUACION
    evaluate :: Tape -> ControlTable -> Tape
    evaluate t code = completeEvaluation (initial, t) code

    completeEvaluation :: Configuration -> ControlTable -> Tape
    completeEvaluation (s, T (l, c, r)) [] = error "There is no code."
    completeEvaluation ("h", t) code = t
    completeEvaluation (s, T (l, c, r)) code = completeEvaluation (step (s, T (l, c, r)) (lookupControlTable s c code)) code

    lookupControlTable :: State -> Symbol -> ControlTable -> TableTuple
    lookupControlTable s c [] = error "Combination of state and symbol not found in code."
    lookupControlTable s c (((q, c2),(a, q')):rcode)
                                    |   c == c2 && s == q = ((q, c2),(a, q'))
                                    |   otherwise = lookupControlTable s c rcode

    -- PRUEBAS
    uno :: Symbol
    uno = "1"

    cero :: Symbol
    cero = "0"

    cinta1 :: Tape
    cinta1 = T ([], blank, [uno, uno, uno, uno, uno, blank])

    cinta2 :: Tape
    cinta2 = T ([uno, blank], blank, [uno, uno, uno, uno, blank, blank, uno])

    pruebaParCinta1 :: Tape
    pruebaParCinta1 = evaluate cinta1 parAlternadoRight; -- Res. Esp: # 1 1 1 1 1 # (F)

    pruebaParCinta2 :: Tape
    pruebaParCinta2 = evaluate cinta2 parAlternadoRight; -- Res. Esp: 1 # # 1 1 1 1 # (T) 1

    cinta3 :: Tape
    cinta3 = T ([], blank, [uno, cero, uno, cero, uno, cero, uno, cero, uno, cero]) 

    cinta4 :: Tape
    cinta4 = T ([uno, "g", "papas"], blank, [uno, cero])

    cinta5 :: Tape
    cinta5 = T ([], blank, [uno, uno, cero, uno, cero, uno, cero])

    pruebaBinAltCinta3 :: Tape
    pruebaBinAltCinta3 = evaluate cinta3 binarioAlternanteRight; -- Res. Esp: # 1 0 1 0 1 0 1 0 1 0 # (T)

    pruebaBinAltCinta4 :: Tape
    pruebaBinAltCinta4 = evaluate cinta4 binarioAlternanteRight; -- Res. Esp: papas g 1 # 1 0 # (T)

    pruebaBinAltCinta5 :: Tape
    pruebaBinAltCinta5 = evaluate cinta5 binarioAlternanteRight; -- Res. Esp: # 1 1 0 1 0 1 0 # (F)

    cinta6 :: Tape
    cinta6 = T ([cero, cero, cero, uno, uno, uno], blank, [uno, cero])

    cinta7 :: Tape
    cinta7 = T ([uno, uno, uno, uno, uno, uno], blank, [])

    cinta8 :: Tape
    cinta8 = T ([cero, blank, cero, cero, cero], blank, [uno])

    pruebaMarkZerosCinta6 :: Tape
    pruebaMarkZerosCinta6 = evaluate cinta6 markZerosLeft -- Res. Esp: (#) 1 1 1 X X X # 1 0 

    pruebaMarkZerosCinta7 :: Tape
    pruebaMarkZerosCinta7 = evaluate cinta7 markZerosLeft -- Res. Esp: (#) 1 1 1 1 1 1 #

    pruebaMarkZerosCinta8 :: Tape
    pruebaMarkZerosCinta8 = evaluate cinta8 markZerosLeft -- Res. Esp: 0 0 0 (#) X # 1

    -- CODIGO TURING
    parAlternadoRight :: ControlTable
    parAlternadoRight = [
        ((initial, blank),(R, "even")),
        (("even", "1"),(R, "odd")),
        (("even", blank),(R, "t")),
        (("odd", "1"),(R, "even")),
        (("odd", blank),(R, "f")),
        (("t", blank),(Sigma "T", halt)),
        (("f", blank),(Sigma "F", halt))
        ]
    
    binarioAlternanteRight :: ControlTable
    binarioAlternanteRight = [
        ((initial, blank),(R, "first")),
        (("first", uno),(R, "zero")),
        (("first", cero),(R, "one")),
        (("first", blank),(R, "accept")),
        (("zero", cero),(R, "one")),
        (("zero", uno),(R, "reject")),
        (("zero", blank),(R, "accept")),
        (("one", uno),(R, "zero")),
        (("one", cero),(R, "reject")),
        (("one", blank),(R, "accept")),
        (("accept", blank),(Sigma "T", halt)),
        (("reject", uno),(R, "reject")),
        (("reject", cero),(R, "reject")),
        (("reject", blank),(R, "f")),
        (("f", blank),(Sigma "F", halt))
        ]

    markZerosLeft :: ControlTable
    markZerosLeft = [
        ((initial, blank),(L, "left")),
        (("left", uno),(L, "left")),
        (("left", cero),(Sigma "X", "left")),
        (("left", "X"),(L, "left")),
        (("left", blank),(Sigma blank, halt))
        ]