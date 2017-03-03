(* ************************* *)
(* **** Tests unitaires **** *)
(* ************************* *)



(* *** Exercice 1 *** *)
print_string("========== Exercice 1 ==========");;

let env1 = {localvar = [("k", IntT); ("n", IntT)]; globalvar = []; returntp = VoidT; funbind = []};;


(* Doit réussir *)
let okConst = Const (0, IntV 1);;

let okVarE = VarE (0, Var (Local, "n"));;

let okSujet = BinOp (0, BCompar BCeq, VarE (0, Var (Local, "n")),
									BinOp (0, BArith BAadd, VarE (0, Var (Local, "k")),
															Const (0, IntV 1)));;

let okLogic = BinOp (0, BLogic BLand, Const (0, BoolV true), Const (0, BoolV true));;

let okIfThenElse = IfThenElse (0, Const (0, BoolV true), Const (0, IntV 1), VarE (0, Var (Local, "n")));;

(* Fonction pour "automatiser" les tests *)
let testSucess = function expRes -> function expOk -> expRes = (tp_expr env1 expOk);;


(* Doit échouer *)
let koVarE = VarE (0, Var (Local, "x"));;

let koBArith = BinOp (0, BArith BAadd, Const (0, BoolV true), Const (0, BoolV true));;
let koBCompar = BinOp (0, BArith BAadd, Const (0, BoolV true), Const (0, IntV 1));;
let koBLogic = BinOp (0, BArith BAadd, Const (0, IntV 1), Const (0, IntV 1));;

(* Constantes de résultat de la fonction test *)
let ok = Const (IntT, IntV 0);;
let ko = Const (IntT, IntV 1);;

(* Fonction pour "automatiser" les tests *)
let testFail = function exp -> try (tp_expr env1 exp)
								with PasDansEnv -> ok | ErreurTypes -> ok | _ -> ko;;


(* Résultats à obtenir *)
let resConst = Const (IntT, IntV 1);;

let resVarE = VarE (IntT, Var (Local, "n"));;

let resSujet = BinOp (BoolT, BCompar BCeq, VarE (IntT, Var (Local, "n")),
											BinOp (IntT, BArith BAadd, VarE (IntT, Var (Local, "k")),
																		Const (IntT, IntV 1)));;

let resLogic = BinOp (BoolT, BLogic BLand, Const (BoolT, BoolV true), Const (BoolT, BoolV true));;

let resIfThenElse = IfThenElse (IntT, Const (BoolT, BoolV true),
								Const (IntT, IntV 1), VarE (IntT, Var (Local, "n")));;


(* Tests *)
print_string("*******************");;
print_string("**** Les tests ****");;

print_string(if testSucess resConst okConst then "Le test à réussi !" else "Le test à échoué...");;
print_string(if testSucess resVarE okVarE then "Le test à réussi !" else "Le test à échoué...");;
print_string(if testSucess resSujet okSujet then "Le test à réussi !" else "Le test à échoué...");;
print_string(if testSucess resLogic okLogic then "Le test à réussi !" else "Le test à échoué...");;
print_string(if testSucess resIfThenElse okIfThenElse then "Le test à réussi !"
														else "Le test à échoué...");;

print_string(if (testFail koVarE) = ok then "Le test à réussi !" else "Le test à échoué...");;
print_string(if (testFail koBArith) = ok then "Le test à réussi !" else "Le test à échoué...");;
print_string(if (testFail koBCompar) = ok then "Le test à réussi !" else "Le test à échoué...");;
print_string(if (testFail koBLogic) = ok then "Le test à réussi !" else "Le test à échoué...");;








