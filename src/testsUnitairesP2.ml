(* ************************************ *)
(* **** Tests unitaires - Partie 2 **** *)
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

let env0 = {localvar = []; globalvar = []; returntp = VoidT; funbind = []};;
let env1 = {localvar = [("n",IntT);("b",BoolT)]; globalvar = []; returntp = VoidT; funbind = []};;

let vars = ["x";"y";"z"];;

let testVS = StringSet.of_list ["x"; "y"];;




(* *********************** *)
(* *** Début des tests *** *)
(* *********************** *)

afficheTexte "\n\t**** PARTIE 2 ****\n";;




(* ****************** *)
(* *** Exercice 6 *** *)
(* ****************** *)

(* Liste de différents stmt à typer *)
let listTestExo6 = [
		Skip; Assign (0, Var (Local, "n"), VarE (0, Var (Local, "n")));
		Seq (Assign (0, Var (Local, "n"), VarE (0, Var (Local, "n"))),
			Assign (0, Var (Local, "n"), VarE (0, Var (Local, "n"))));
		Cond (Const (0, BoolV true), Assign (0, Var (Local, "n"), VarE (0, Var (Local, "n"))),
			Assign (0, Var (Local, "n"), VarE (0, Var (Local, "n"))));
		While (Const (0, BoolV true), Assign (0, Var (Local, "n"), VarE (0, Var (Local, "n"))));
		CallC ("f", [Const (0, IntV 1);VarE (0, Var (Local, "n"))]);
		Return (Const (0, IntV 1))
	];;


(*  Liste des différents stmt typés
	C'est le résultat qui doit être obtenus en appliquant tp_stmt à listTestExo6 *)
let listOkExo6 = [
		Skip; Assign (VoidT, Var (Local, "n"), VarE (IntT, Var (Local, "n")));
		Seq (Assign (VoidT, Var (Local, "n"), VarE (IntT, Var (Local, "n"))),
			Assign (VoidT, Var (Local, "n"), VarE (IntT, Var (Local, "n"))));
		Cond (Const (BoolT, BoolV true), Assign (VoidT, Var (Local, "n"), VarE (IntT, Var (Local, "n"))),
			Assign (VoidT, Var (Local, "n"), VarE (IntT, Var (Local, "n"))));
		While (Const (BoolT, BoolV true), Assign (VoidT, Var (Local, "n"), VarE (IntT, Var (Local, "n"))));
		CallC ("f", [Const (IntT, IntV 1);VarE (IntT, Var (Local, "n"))]);
		Return (Const (IntT, IntV 1))
	];;


(* Lance le test et affiche le résultat *)
afficheTexte "==== Exercice 6 ====";
resTest "tp_stmt" (listesEgales (List.map (tp_stmt env1) listTestExo6, listOkExo6));;




(* ****************** *)
(* *** Exercice 7 *** *)
(* ****************** *)

(* Fonction à typer *)
let testExo7 = Fundefn (
	Fundecl (IntT, "f", [Vardecl (IntT, "n")]),
	[Vardecl (IntT, "n")],
	Seq (Skip, Return (Const (0, IntV 1))));;


(* Résultats à obtenir *)
let okExo7 = Fundefn (
	Fundecl (IntT, "f", [Vardecl (IntT, "n")]),
	[Vardecl (IntT, "n")],
	Seq (Skip, Return (Const (IntT, IntV 1))));;


(* Résultats à ne pas obtenir *)
let koExo7 = Fundefn (
	Fundecl (IntT, "f", [Vardecl (IntT, "n")]),
	[Vardecl (IntT, "n")],
	Seq (Skip, Return (Const (BoolT, BoolV true))));;


(* Doit lever une exception *)
let exExo7 = Fundefn (
	Fundecl (IntT, "f", [Vardecl (IntT, "n")]),
	[Vardecl (IntT, "n")],
	Assign (0, Var (Local, "n"), VarE (0, Var (Local, "n"))));;


(* Lance les tests et affiche le résultat *)
afficheTexte "";;
afficheTexte "==== Exercice 7 ====";
resTest "Avec type de retour bon" ((tp_fdefn env0 testExo7) = okExo7);;
resTest "Avec type de retour mauvais" ((tp_fdefn env0 testExo7) <> koExo7);;
resTest "Avec levé d'exception" (try (tp_fdefn env0 exExo7) = (tp_fdefn env0 exExo7) with
								 FonctionMalTypee -> true | _ -> false);;




(* ****************** *)
(* *** Exercice 8 *** *)
(* ****************** *)

(* Programme à typer *)
let testExo8 = Prog([Vardecl (IntT, "n"); Vardecl (BoolT, "b")], [(Fundefn (
	Fundecl (IntT, "f", [Vardecl (IntT, "n"); Vardecl (BoolT, "b")]),
	[Vardecl (IntT, "n"); Vardecl (BoolT, "b")],
	Seq (Skip, Return (Const (0, IntV 1)))))]);;


(* Résultats à obtenir *)
let okExo8 = Prog([], [(Fundefn (
	Fundecl (IntT, "f", [Vardecl (IntT, "n"); Vardecl (BoolT, "b")]),
	[Vardecl (IntT, "n"); Vardecl (BoolT, "b")],
	Seq (Skip, Return (Const (IntT, IntV 1)))))]);;


(* Résultats à ne pas obtenir *)
let koExo8 = Prog([Vardecl (IntT, "n"); Vardecl (BoolT, "b")], [(Fundefn (
	Fundecl (IntT, "f", [Vardecl (IntT, "n"); Vardecl (BoolT, "b")]),
	[Vardecl (IntT, "n"); Vardecl (BoolT, "b")],
	Seq (Skip, Return (Const (IntT, IntV 1)))))]);;


(* Doit lever une exception *)
let exExo8 = Prog([Vardecl (IntT, "n"); Vardecl (BoolT, "b")], [(Fundefn (
	Fundecl (IntT, "f", [Vardecl (IntT, "n"); Vardecl (BoolT, "b")]),
	[Vardecl (IntT, "n"); Vardecl (BoolT, "b")],
	Seq (Skip, Return (Const (0, BoolV true)))))]);;


(* Lance les tests et affiche le résultat *)
afficheTexte "";;
afficheTexte "==== Exercice 8 ====";
resTest "Avec programme bon" ((tp_prog testExo8) = okExo8);;
resTest "Avec programme mauvais" ((tp_prog testExo8) <> koExo8);;
resTest "Avec fonction mal typée" (try (tp_prog exExo8) = (tp_prog exExo8) with
								   ProgrammeMauvais -> true | _ -> false);;




(* ****************** *)
(* *** Exercice 9 *** *)
(* ****************** *)

(* Test IfThenElse *)
let ieTrue = Const (BoolT, (BoolV true));;
let ieFalse = Const (BoolT, (BoolV false));;
let ieTest = IfThenElse (IntT, ieFalse, (Const (IntT, (IntV 444))), (Const (IntT, (IntV 555))));;
let i1test = IfThenElse (IntT, ieTrue, (Const (IntT, (IntV 666))), (Const (IntT, (IntV 777))));;
let i2test = IfThenElse (IntT, ieTrue, (Const (IntT, (IntV 888))), (Const (IntT, (IntV 999))));;
let ifTest = IfThenElse (IntT, ieTest, i1test, i2test);;

let okIf = [Loadc (BoolT, BoolV false); Loadc (IntT, IntV 0); If (BCeq, [2; 0]);
 Loadc (IntT, IntV 444); Goto [2; 2]; Label [2; 0]; Loadc (IntT, IntV 555);
 Label [2; 2]; Loadc (IntT, IntV 0); If (BCeq, [0]);
 Loadc (BoolT, BoolV true); Loadc (IntT, IntV 0); If (BCeq, [1; 0]);
 Loadc (IntT, IntV 666); Goto [1; 2]; Label [1; 0]; Loadc (IntT, IntV 777);
 Label [1; 2]; Goto [2]; Label [0]; Loadc (BoolT, BoolV true);
 Loadc (IntT, IntV 0); If (BCeq, [0; 0]); Loadc (IntT, IntV 888);
 Goto [0; 2]; Label [0; 0]; Loadc (IntT, IntV 999); Label [0; 2]; Label [2]];;

(* Test CallE *)
let testCE = CallE (IntT, "add2", [Const (IntT, (IntV 2)) ; Const (IntT, (IntV 4))]);;
let okCE = [Loadc (IntT, IntV 2); Loadc (IntT, IntV 4); Invoke (IntT, "add2", [IntT; IntT]); ReturnI IntT];;


afficheTexte "";;
afficheTexte "==== Exercice 9 ====";
resTest "IfThenElse" ((gen_expr vars [] ifTest) = okIf);;
resTest "CallE" ((gen_expr vars [] testCE) = okCE);;




(* ******************* *)
(* *** Exercice 10 *** *)
(* ******************* *)

let testSeqCond10 = Seq (Skip, Cond (Const (BoolT, (BoolV true)),
									Assign (VoidT, Var (Local, "x"), Const (IntT, IntV 0))
									, Skip));;

let testWhileAssign10 = While (
	BinOp (BoolT, BCompar BCne, VarE (IntT, Var (Local, "x")), Const (IntT, (IntV 5)))
	, Assign (VoidT, Var (Local, "x"),
				(BinOp (IntT, BArith BAadd, VarE (IntT, Var (Local, "x")), Const (IntT, IntV 1)))
		));;

let testCallC10 = CallC ("add2", [Const (IntT, (IntV 2)) ; Const (IntT, (IntV 4))]);;

let testReturn10 = Return (VarE (IntT, Var (Local, "x")));;


let okSeqCond10 = [Nop; Loadc (BoolT, BoolV true); Loadc (IntT, IntV 0); If (BCeq, [0]);
 					Loadc (IntT, IntV 0); Storev (VoidT, 0); Goto [2]; Label [0]; Nop;
 					Label [2]];;

let okWhileAssign10 = [Label [1]; Loadv (IntT, 0); Loadc (IntT, IntV 5);
					   Bininst (BoolT, BCompar BCne); Loadc (IntT, IntV 0); If (BCeq, [0]);
					   Loadv (IntT, 0); Loadc (IntT, IntV 1); Bininst (IntT, BArith BAadd);
					   Storev (VoidT, 0); Goto [1]; Label [0]; Nop];;

let okCallC10 = [Loadc (IntT, IntV 2); Loadc (IntT, IntV 4);
				  Invoke (VoidT, "add2", [IntT; IntT]); ReturnI VoidT];;

let okReturn10 = [ReturnI IntT];;


afficheTexte "";;
afficheTexte "==== Exercice 10 ====";
resTest "Seq & Cond" ((gen_stmt vars [] testSeqCond10) = okSeqCond10);;
resTest "While & Assign" ((gen_stmt vars [] testWhileAssign10) = okWhileAssign10);;
resTest "CallC" ((gen_stmt vars [] testCallC10) = okCallC10);;
resTest "Return" ((gen_stmt vars [] testReturn10) = okReturn10);;




(* ******************* *)
(* *** Exercice 11 *** *)
(* ******************* *)

let corpsFundefn = Assign (VoidT, Var (Local, "x"),
	(BinOp (IntT, BArith BAadd, VarE (IntT, Var (Local, "x")), VarE (IntT, Var (Local, "n")))));;

let testFundefn = Fundefn (Fundecl (IntT, "add_n_to_x", [Vardecl (IntT, "n")]),
							[Vardecl (IntT, "x")], corpsFundefn);;


let okFundefn = Methdefn (Methdecl (IntT, "add_n_to_x", [IntT]), Methinfo (2, 1),
						  [Loadv (IntT, 0); Loadv (IntT, 1); Bininst (IntT, BArith BAadd); Storev (VoidT, 0)]);;


afficheTexte "";;
afficheTexte "==== Exercice 11 ====";
resTest "Fundefn" ((gen_fundefn testFundefn) = okFundefn);;




(* ******************* *)
(* *** Exercice 12 *** *)
(* ******************* *)

let testReturn12 = Return (Const (BoolT, (BoolV true)));;

let testAssign12 = Assign (VoidT, Var (Local, "x"), (Const (IntT, (IntV 1))));;

let testSeq12 = Seq (Skip, testReturn12);;

let testCond12 = Cond ((Const (BoolT, (BoolV true))), Skip, testReturn12);;

let testWhile12 = While ((Const (BoolT, (BoolV true)), testReturn12));;

let testCallC12 = CallC ("add2", [Const (IntT, (IntV 2)) ; Const (IntT, (IntV 4))]);;


afficheTexte "";;
afficheTexte "==== Exercice 12 ====";
resTest "Skip" (not(stmt_returns Skip));;
resTest "Assign" (not(stmt_returns testAssign12));;
resTest "Seq" (stmt_returns testSeq12);;
resTest "Cond" (stmt_returns testCond12);;
resTest "While" (stmt_returns testWhile12);;
resTest "CallC" (stmt_returns testCallC12);;




(* ******************* *)
(* *** Exercice 13 *** *)
(* ******************* *)

let testConst13 = Const (BoolT, (BoolV true));;

let testVarE13 = VarE (IntT, Var (Local, "x"));;

let testBinOp13 = BinOp (IntT, BArith BAadd, testVarE13, Const (IntT, (IntV 1)));;

let testIfThenElse13 = IfThenElse (IntT, testConst13, (Const (IntT, (IntV 444))), (Const (IntT, (IntV 555))));;

let testCallE13 = CallE (IntT, "add2", [Const (IntT, (IntV 2)) ; Const (IntT, (IntV 4))]);;

let testReturn13 = Return (testConst13);;

let testAssign13 = Assign (VoidT, Var (Local, "x"), (Const (IntT, (IntV 1))));;

let testSeq13 = Seq (Skip, testReturn13);;

let testCond13 = Cond ((Const (BoolT, (BoolV true))), Skip, testReturn13);;

let testWhile13 = While ((Const (BoolT, (BoolV true)), testReturn13));;

let testCallC13 = CallC ("add2", [Const (IntT, (IntV 2)) ; Const (IntT, (IntV 4))]);;


afficheTexte "";;
afficheTexte "==== Exercice 13 ====";
resTest "Const" ((stack_depth_e testConst13) = 1);;
resTest "VarE" ((stack_depth_e testVarE13) = 1);;
resTest "BinOp" ((stack_depth_e testBinOp13) = 1);;
resTest "IfThenElse" ((stack_depth_e testIfThenElse13) = 2);;
resTest "CallE" ((stack_depth_e testCallE13) = 2);;
resTest "Return" ((stack_depth_c testReturn13) = 1);;
resTest "Assign" ((stack_depth_c testAssign13) = 1);;
resTest "Seq" ((stack_depth_c testSeq13) = 1);;
resTest "Cond" ((stack_depth_c testCond13) = 2);;
resTest "While" ((stack_depth_c testWhile13) = 2);;
resTest "CallC" ((stack_depth_c testWhile13) = 2);;




(* ******************* *)
(* *** Exercice 14 *** *)
(* ******************* *)

let okExpr14 = VarE (IntT, Var (Local, "x"));;

let koExpr14 = VarE (IntT, Var (Local, "z"));;

let okInst14 = Return okExpr14;;

let koInst14 = Return koExpr14;;


afficheTexte "";;
afficheTexte "==== Exercice 14 ====";
resTest "defassign_e" ((defassign_e testVS okExpr14) && not(defassign_e testVS koExpr14));;
resTest "defassign_c" ((defassign_c testVS okInst14) = testVS &&
	try (defassign_c testVS koInst14) = testVS with ExpressionIndefinie -> true | _ -> false);;

















