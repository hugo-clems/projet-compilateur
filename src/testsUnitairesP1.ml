(* ************************************ *)
(* **** Tests unitaires - Partie 1 **** *)
(* ************************************ *)

#load "analyses.cmo";;
#load "lang.cmo";;

#use "lang.ml";;
#use "analyses.ml";;
#use "typing.ml";;
#use "gen.ml";;




(* ****************** *)
(* *** Constantes *** *)
(* ****************** *)

(**
 * Permet de vérifier que 2 listes sont égales
 * @param (liste1,liste2) les listes à comparer
 * @return true si les listes sont égales, false sinon
 *)
let rec listesEgales = function
	| ([],[]) -> true
	| ([],_) -> false
	| (_,[]) -> false
	| ((a::resteA),(b::resteB)) -> if a = b then listesEgales(resteA,resteB) else false;;


(**
 * Affiche le résultat d'un test
 * @param precisions - petit texte de précisions par rapport à ce qui est testé
 * @param estReussi - true si le test est réussi, false sinon
 *)
let resTest = function precisions -> function estReussi ->
	if estReussi then print_string("TEST REUSSI :\t" ^ precisions ^ "\n")
	else print_string("/!\\ ECHEC DU TEST :\t" ^ precisions ^ "\n");;


(**
 * Affiche du texte dans la console lors de l'exécution du code
 * @param txt - le texte à afficher
 *)
let afficheTexte = function txt -> print_string(txt ^ "\n");;


(* Divers *)
let f1 = Fundecl (IntT, "f", [Vardecl (IntT, "n"); Vardecl (BoolT, "b")]);;

let env2 = {localvar = [("k", IntT); ("n", IntT)]; globalvar = []; returntp = VoidT; funbind = [f1]};;

let vars = ["x";"y";"z"];;




(* *********************** *)
(* *** Début des tests *** *)
(* *********************** *)

afficheTexte "\n\t**** PARTIE 1 ****\n";;




(* ********************** *)
(* *** Exercice 1 & 2 *** *)
(* ********************** *)

(* *** Les tests qui doivent réussir *** *)
let okConst = Const (0, IntV 1);;

let okVarE = VarE (0, Var (Local, "n"));;

let okSujet = BinOp (0, BCompar BCeq, VarE (0, Var (Local, "n")),
									BinOp (0, BArith BAadd, VarE (0, Var (Local, "k")),
															Const (0, IntV 1)));;

let okLogic = BinOp (0, BLogic BLand, Const (0, BoolV true), Const (0, BoolV true));;

let okIfThenElse = IfThenElse (0, Const (0, BoolV true), Const (0, IntV 1), VarE (0, Var (Local, "n")));;

let okCallE = CallE(0, "f", [Const (0, IntV 3); Const (0, BoolV true)]);;


(* *** Les résultats à obtenir *** *)
let resConst = Const (IntT, IntV 1);;

let resVarE = VarE (IntT, Var (Local, "n"));;

let resSujet = BinOp (BoolT, BCompar BCeq, VarE (IntT, Var (Local, "n")),
											BinOp (IntT, BArith BAadd, VarE (IntT, Var (Local, "k")),
																		Const (IntT, IntV 1)));;

let resLogic = BinOp (BoolT, BLogic BLand, Const (BoolT, BoolV true), Const (BoolT, BoolV true));;

let resIfThenElse = IfThenElse (IntT, Const (BoolT, BoolV true),
								Const (IntT, IntV 1), VarE (IntT, Var (Local, "n")));;

let resCallE = CallE(IntT, "f", [Const (IntT, IntV 3); Const (BoolT, BoolV true)]);;


(* *** Les tests qui doivent lever une exception *** *)
let koVarE = VarE (0, Var (Local, "x"));;

let koBArith = BinOp (0, BArith BAadd, Const (0, BoolV true), Const (0, BoolV true));;

let koBCompar = BinOp (0, BCompar BCeq, Const (0, BoolV true), Const (0, IntV 1));;

let koBLogic = BinOp (0, BLogic BLand, Const (0, IntV 1), Const (0, IntV 1));;

let koCallEMauvaiseFct = CallE(0, "g", [Const (0, IntV 3); Const (0, BoolV true)]);;

let koCallEMauvaisArg = CallE(0, "f", [Const (0, BoolV true); Const (0, BoolV true)]);;

(* Pour "automatiser" les tests *)
let testKoTpExpr = function exp -> try (tp_expr env2 exp) = Const (IntT, IntV 1)
								with PasDansEnv -> true | ErreurTypes -> true
								| MauvaisArguments -> true | Not_found -> true
								| _ -> false;;


(* *** Lancement des tests *** *)
afficheTexte "==== Exercice 1 ====";;
resTest "Const" ((tp_expr env2 okConst) = resConst);;
resTest "VarE" ((tp_expr env2 okVarE) = resVarE && (testKoTpExpr koVarE));;
resTest "Exemple du sujet" ((tp_expr env2 okSujet) = resSujet);;
resTest "BLogic" ((tp_expr env2 okLogic) = resLogic && (testKoTpExpr koBLogic));;
resTest "Barith" (testKoTpExpr koBArith);;
resTest "BCompar" (testKoTpExpr koBCompar);;
resTest "IfThenElse" ((tp_expr env2 okIfThenElse) = resIfThenElse);;

afficheTexte "";;
afficheTexte "==== Exercice 2 ====";;
resTest "CallE" ((tp_expr env2 okCallE) = resCallE);;
resTest "CallE - MauvaiseFct" (testKoTpExpr koCallEMauvaiseFct);;
resTest "CallE - MauvaisArg" (testKoTpExpr koCallEMauvaisArg);;




(* ****************** *)
(* *** Exercice 3 *** *)
(* ****************** *)

(* *** Les tests qui doivent réussir *** *)
let okPosition1 = position (Const (IntT, IntV 1)) [(Const (IntT, IntV 1));(Const (IntT, IntV 8))];;

let okPosition2 = position 1 [0;1;2;3];;

let okPosition3 = position 'x' ['y';'e';'x'];;


(* *** Les résultats à obtenir *** *)
let resPosition1 = 0;;

let resPosition2 = 1;;

let resPosition3 = 2;;


(* *** Le test qui doit lever une exception *** *)
let koPositionA = "pasdanslaliste";;
let koPositionB = ["b";"hj";"p";"liste"];;


(* *** Lancement des tests *** *)
afficheTexte "";;
afficheTexte "==== Exercice 3 ====";;
resTest "Position" (okPosition1 = resPosition1 && okPosition2 = resPosition2 && okPosition3 = resPosition3);;
resTest "Position - PasDansLaliste" (try (position koPositionA koPositionB) = 0
									 with PasDansLaliste -> true | _ -> false);;




(* ****************** *)
(* *** Exercice 4 *** *)
(* ****************** *)

(* *** Les tests qui doivent réussir *** *)
let okGConst = Const (IntT, IntV 3);;

let okGVarE = VarE (IntT, Var (Local, "x"));;

let okGBinOp = BinOp (IntT, BArith BAadd, Const (IntT, IntV 3), VarE (IntT, Var (Local, "x")));;


(* *** Les résultats à obtenir *** *)
let resGConst = [Loadc (IntT, IntV 3)];;

let resGVarE = [Loadv (IntT, 0)];;

let resGBinOp = [Loadc (IntT, IntV 3) ; Loadv (IntT, 0) ; Bininst (IntT, BArith BAadd)];;


(* *** Le test qui doit lever une exception *** *)
let koGVarE = VarE (IntT, Var (Local, "n"));;


(* *** Lancement des tests *** *)
afficheTexte "";;
afficheTexte "==== Exercice 4 ====";;
resTest "gen_expr Const" (listesEgales((gen_expr vars [] okGConst), resGConst));;
resTest "gen_expr VarE" (listesEgales((gen_expr vars [] okGVarE), resGVarE));;
resTest "gen_expr BinOp" (listesEgales((gen_expr vars [] okGBinOp), resGBinOp));;
resTest "gen_expr - PasDansLaliste" (try listesEgales((gen_expr vars [] koGVarE), [])
									 with PasDansLaliste -> true | _ -> false);;
