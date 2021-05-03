module CalcLambdaPuroEmbed where

import Tarea1

-- typExpr Var = String
-- typExpr Id = String
-- typExpr B = (Id, ([Var], Expr))

-- data Expr = Var Var | C Id | Lam [Var] Expr | Expr :- [Expr] | CasExpr Expr [B] | RExprc Expr
--     dExprriving(Show)

c_0::Expr
c_0 = Lam["f"] (Lam["x"] (Var "x"))

c_1::Expr
c_1 = Lam["f"] (Lam["x"] ( Apl (Var "f") [Var "x"]))

c_2::Expr
c_2 = Lam["f"] (Lam["x"] (Apl (Var "f") [Apl (Var "f") [Var "x"]]))

c_3::Expr
c_3 = Lam["f"] (Lam["x"] (Apl (Var "f") [Apl (Var "f") [Apl (Var "f") [Var "x"]]]))

c_4::Expr
c_4 = Lam["f"] (Lam["x"] (Apl (Var "f") [Apl (Var "f") [Apl (Var "f") [Apl (Var "f") [Var "x"]]]]))

c_plus :: Expr
c_plus = Lam["m"] (Lam["n"] (Lam["f"] (Lam["x"] ((Var "m" :- [Var "f"]):- [(Var "n" :- [Var "f"]):-[Var "x"]]))))

c_1_plus_2 :: Expr
c_1_plus_2 = evaluacionFuerte ((c_plus :- [c_1]):-[c_2])

c_1_plus_2_S_O :: Expr
c_1_plus_2_S_O = evaluacionFuerte ((((c_plus :- [c_1]):-[c_2]):- [C"S"]):-[C"O"])

--t = \Var.(\y. Var)
t :: Expr
t = Lam ["x"] (Lam ["y"] (Var "x"))

--f = \Var.(\y. y)
f :: Expr
f = Lam ["x"] (Lam ["y"] (Var "y"))

--not = \b.(\Var.\y.(b [y])[Var])
nnot :: Expr
nnot = Lam["b"] (Lam["x"] (Lam["y"] ((Var "b" :- [Var"y"]) :- [Var"x"])))

nnot_t :: Expr
nnot_t = evaluacionFuerte (nnot :- [t])
nnot_f :: Expr
nnot_f = evaluacionFuerte (nnot :- [f])

nnot_t_T_F :: Expr
nnot_t_T_F = evaluacionFuerte (((nnot :- [t]) :- [C "TruExpr"]):-[C"FaLamsExpr"])

nnot_f_T_F :: Expr
nnot_f_T_F = evaluacionFuerte (((nnot :- [f]) :- [C "TruExpr"]):-[C"FaLamsExpr"])

c_mul :: Expr
c_mul = Lam["m"] (Lam["n"]  (Lam["f"] (Var"m":-[Var"n":-[Var"f"]])))

pair :: Expr
pair = Lam["x"] (Lam["y"] (Lam["f"] ((Var "f" :- [Var"x"]):- [Var"y"])))

first :: Expr
first = Lam["p"] (Var"p" :- [t])

sExprcond :: Expr
sExprcond = Lam["p"] (Var"p" :- [f])

s::Expr
s = Lam["n"] (Lam["f"] (Lam["x"] (Var "f" :- [(Var "n" :- [Var"f"]):- [Var "x"]])))

s' :: Expr
s' = Lam ["p"] ((pair :- [sExprcond :- [Var"p"]]):- [s :- [sExprcond :- [Var"p"]]])

p :: Expr
p = Lam["n"] (first :- [(Var "n" :- [s']):-[(pair :- [c_0]):-[c_0]]])

c_ :: Expr
c_ = Lam["m"] (Lam["n"] ((Var"n" :- [p]):- [Var"m"]))

p_4_minus_1 = evaluacionFuerte ((((c_:-[c_4]):- [c_1]):-[C"S"]):-[C"O"])

y :: Expr
y= Lam["f"](Lam["x"] (Var "x" :- [(Var"f" :- [Var"f"]) :- [Var "x"]])) :- [Lam["f"](Lam["x"] (Var "x" :- [(Var"f" :- [Var"f"]) :- [Var "x"]]))]

if_0 :: Expr
if_0 = Lam["n"] ((Var"n" :- [Lam["x"] (f)]):-[t])

sum_upto :: Expr
sum_upto = (y:-[Lam["r"] (Lam["n"] (((if_0 :-[Var"n"]):-[c_0]):-[(c_plus:-[Var"n"]):-[Var"r" :- [p :- [Var"n"]]]]))])

exp :: Expr
exp = (y:-[Lam["r"] (Lam["n"] (((if_0 :-[Var"n"]):-[c_1]):-[(c_muLam:-[Var"n"]):-[Var"r" :- [p :- [Var"n"]]]]))])