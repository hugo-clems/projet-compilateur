(* ************************************ *)
(* **** Tests unitaires - Partie 1 **** *)
(* ************************************ *)

#load "analyses.cmo";;
#load "lang.cmo";;

#use "lang.ml";;
#use "typing.ml";;
#use "gen.ml";;

(* Constantes *)
let okTpExpr = Const (IntT, IntV 0);;
let koTpExpr = Const (IntT, IntV 1);;
let okG = [Loadc (IntT, IntV 0)];;
let koG = [Loadc (IntT, IntV 1)];;
let msgOk = "Le test à réussi !\n";;
let msgKo = "Le test à échoué...\n";;

(* Permet de vérifier que 2 listes sont égales *)
let rec listesEgales = function
	| ([],[]) -> true
	| ([],_) -> false
	| (_,[]) -> false
	| ((a::resteA),(b::resteB)) -> if a = b then listesEgales(resteA,resteB) else false;;




(* ********************** *)
(* *** Exercice 1 & 2 *** *)
(* ********************** *)


let f1 = Fundecl (IntT, "f", [Vardecl (IntT, "n"); Vardecl (BoolT, "b")]);;
let env1 = {localvar = [("k", IntT); ("n", IntT)]; globalvar = []; returntp = VoidT;
			funbind = [f1]};;


(* Doit réussir *)
let okConst = Const (0, IntV 1);;

let okVarE = VarE (0, Var (Local, "n"));;

let okSujet = BinOp (0, BCompar BCeq, VarE (0, Var (Local, "n")),
									BinOp (0, BArith BAadd, VarE (0, Var (Local, "k")),
															Const (0, IntV 1)));;

let okLogic = BinOp (0, BLogic BLand, Const (0, BoolV true), Const (0, BoolV true));;

let okIfThenElse = IfThenElse (0, Const (0, BoolV true), Const (0, IntV 1), VarE (0, Var (Local, "n")));;

let okCallE = CallE(0, "f", [Const (0, IntV 3); Const (0, BoolV true)]);;

(* Fonction pour "automatiser" les tests *)
let testOkTpExpr = function expRes -> function expOk -> expRes = (tp_expr env1 expOk);;


(* Doit échouer *)
let koVarE = VarE (0, Var (Local, "x"));;

let koBArith = BinOp (0, BArith BAadd, Const (0, BoolV true), Const (0, BoolV true));;

let koBCompar = BinOp (0, BCompar BCeq, Const (0, BoolV true), Const (0, IntV 1));;

let koBLogic = BinOp (0, BLogic BLand, Const (0, IntV 1), Const (0, IntV 1));;

let koCallEMauvaiseFct = CallE(0, "g", [Const (0, IntV 3); Const (0, BoolV true)]);;

let koCallEMauvaisArg = CallE(0, "f", [Const (0, BoolV true); Const (0, BoolV true)]);;

(* Fonction pour "automatiser" les tests *)
let testKoTpExpr = function exp -> try (tp_expr env1 exp)
								with PasDansEnv -> okTpExpr | ErreurTypes -> okTpExpr
								| MauvaisArguments -> okTpExpr | Not_found -> okTpExpr
								| _ -> koTpExpr;;


(* Résultats à obtenir *)
let resConst = Const (IntT, IntV 1);;

let resVarE = VarE (IntT, Var (Local, "n"));;

let resSujet = BinOp (BoolT, BCompar BCeq, VarE (IntT, Var (Local, "n")),
											BinOp (IntT, BArith BAadd, VarE (IntT, Var (Local, "k")),
																		Const (IntT, IntV 1)));;

let resLogic = BinOp (BoolT, BLogic BLand, Const (BoolT, BoolV true), Const (BoolT, BoolV true));;

let resIfThenElse = IfThenElse (IntT, Const (BoolT, BoolV true),
								Const (IntT, IntV 1), VarE (IntT, Var (Local, "n")));;

let resCallE = CallE(IntT, "f", [Const (IntT, IntV 3); Const (BoolT, BoolV true)]);;




(* ****************** *)
(* *** Exercice 3 *** *)
(* ****************** *)


let okPosition1 = position (Const (IntT, IntV 1)) [(Const (IntT, IntV 1));(Const (IntT, IntV 8))];;
let okPosition2 = position 1 [0;1;2;3];;
let okPosition3 = position 'x' ['y';'e';'x'];;

let koPosition1 = ("pasdanslaliste",["b";"hj";"c";"a"]);;

let resPosition1 = 0;;
let resPosition2 = 1;;
let resPosition3 = 2;;

let testOkPosition = function leTest -> function leRes -> leTest = leRes;;
let testKoPosition = function (a,b) -> try position a b with PasDansLaliste -> 0 | _ -> 1;;




(* ****************** *)
(* *** Exercice 4 *** *)
(* ****************** *)


let okGConst = Const (IntT, IntV 3);;
let okGVarE = VarE (IntT, Var (Local, "x"));;
let okGBinOp = BinOp (IntT, BArith BAadd, Const (IntT, IntV 3), VarE (IntT, Var (Local, "x")));;

let koGVarE = VarE (IntT, Var (Local, "n"));;

let resGConst = [Loadc (IntT, IntV 3)];;
let resGVarE = [Loadv (IntT, 0)];;
let resGBinOp = [Loadc (IntT, IntV 3) ; Loadv (IntT, 0) ; Bininst (IntT, BArith BAadd)];;

let testOkG = function res -> function sol -> listesEgales(res, (gen_expr ["x";"y";"z"] [] sol));;
let testKoG = function test -> try (gen_expr ["x";"y";"z"] [] test)
								with PasDansLaliste -> okG | _ -> koG;;




(* *************************** *)
(* *** Lancement des tests *** *)
(* *************************** *)
print_string("========== Exercice 1 ==========\n");;
print_string(if testOkTpExpr resConst okConst then msgOk else msgKo);;
print_string(if testOkTpExpr resVarE okVarE then msgOk else msgKo);;
print_string(if testOkTpExpr resSujet okSujet then msgOk else msgKo);;
print_string(if testOkTpExpr resLogic okLogic then msgOk else msgKo);;
print_string(if testOkTpExpr resIfThenElse okIfThenElse then msgOk else msgKo);;
print_string(if testOkTpExpr resIfThenElse okIfThenElse then msgOk else msgKo);;
print_string(if (testKoTpExpr koVarE) = okTpExpr then msgOk else msgKo);;
print_string(if (testKoTpExpr koBArith) = okTpExpr then msgOk else msgKo);;
print_string(if (testKoTpExpr koBCompar) = okTpExpr then msgOk else msgKo);;
print_string(if (testKoTpExpr koBLogic) = okTpExpr then msgOk else msgKo);;

print_string("========== Exercice 2 ==========\n");;
print_string(if testOkTpExpr resCallE okCallE then msgOk else msgKo);;
print_string(if (testKoTpExpr koCallEMauvaisArg) = okTpExpr then msgOk else msgKo);;
print_string(if (testKoTpExpr koCallEMauvaiseFct) = okTpExpr then msgOk else msgKo);;

print_string("========== Exercice 3 ==========\n");;
print_string(if testOkPosition okPosition1 resPosition1 then msgOk else msgKo);;
print_string(if testOkPosition okPosition2 resPosition2 then msgOk else msgKo);;
print_string(if testOkPosition okPosition3 resPosition3 then msgOk else msgKo);;
print_string(if (testKoPosition koPosition1) == 0 then msgOk else msgKo);;

print_string("========== Exercice 4 ==========\n");;
print_string(if (testOkG resGConst okGConst) then msgOk else msgKo);;
print_string(if (testOkG resGVarE okGVarE) then msgOk else msgKo);;
print_string(if (testOkG resGBinOp okGBinOp) then msgOk else msgKo);;
print_string(if listesEgales((testKoG koGVarE), okG) then msgOk else msgKo);;










