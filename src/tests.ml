(* ************************* *)
(* **** Tests unitaires **** *)
(* ************************* *)

#use "lang.ml";;
#use "typing.ml";;

(* Constantes *)
let ok = Const (IntT, IntV 0);;
let ko = Const (IntT, IntV 1);;
let msgOk = "Le test à réussi !\n";;
let msgKo = "Le test à échoué...\n";;




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
let testSuccess = function expRes -> function expOk -> expRes = (tp_expr env1 expOk);;


(* Doit échouer *)
let koVarE = VarE (0, Var (Local, "x"));;

let koBArith = BinOp (0, BArith BAadd, Const (0, BoolV true), Const (0, BoolV true));;

let koBCompar = BinOp (0, BCompar BCeq, Const (0, BoolV true), Const (0, IntV 1));;

let koBLogic = BinOp (0, BLogic BLand, Const (0, IntV 1), Const (0, IntV 1));;

let koCallEMauvaiseFct = CallE(0, "g", [Const (0, IntV 3); Const (0, BoolV true)]);;

let koCallEMauvaisArg = CallE(0, "f", [Const (0, BoolV true); Const (0, BoolV true)]);;

(* Fonction pour "automatiser" les tests *)
let testFail = function exp -> try (tp_expr env1 exp)
								with PasDansEnv -> ok | ErreurTypes -> ok
								| MauvaisArguments -> ok | _ -> ko;;


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


(* Tests *)
print_string("========== Exercice 1 ==========\n");;
print_string(if testSuccess resConst okConst then msgOk else msgKo);;
print_string(if testSuccess resVarE okVarE then msgOk else msgKo);;
print_string(if testSuccess resSujet okSujet then msgOk else msgKo);;
print_string(if testSuccess resLogic okLogic then msgOk else msgKo);;
print_string(if testSuccess resIfThenElse okIfThenElse then msgOk else msgKo);;
print_string(if testSuccess resIfThenElse okIfThenElse then msgOk else msgKo);;
print_string(if (testFail koVarE) = ok then msgOk else msgKo);;
print_string(if (testFail koBArith) = ok then msgOk else msgKo);;
print_string(if (testFail koBCompar) = ok then msgOk else msgKo);;
print_string(if (testFail koBLogic) = ok then msgOk else msgKo);;

print_string("========== Exercice 2 ==========\n");;
print_string(if testSuccess resCallE okCallE then msgOk else msgKo);;
print_string(if (testFail koCallEMauvaisArg) = ok then msgOk else msgKo);;
print_string(if (testFail koCallEMauvaiseFct) = ok then msgOk else msgKo);;







