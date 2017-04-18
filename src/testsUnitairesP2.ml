(* ************************************ *)
(* **** Tests unitaires - Partie 2 **** *)
(* ************************************ *)

#use "lang.ml";;
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
 * @param exo - le nom de l'exercice testé
 * @param estReussi - true si le test est réussi, false sinon
 *)
let resTest = function exo -> function estReussi ->
	if estReussi then print_string("TEST REUSSI :\t" ^ exo ^ "\n")
	else print_string("/!\\ ECHEC DU TEST :\t" ^ exo ^ "\n");;


(* *** Environements pour les tests *** *)
let env0 = {localvar = []; globalvar = []; returntp = VoidT; funbind = []};;
let env1 = {localvar = [("n",IntT);("b",BoolT)]; globalvar = []; returntp = VoidT; funbind = []};;




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
resTest "Exercice 6 - tp_stmt" (listesEgales (List.map (tp_stmt env1) listTestExo6, listOkExo6));;




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


(* Lance le test et affiche le résultat *)
resTest "Exercice 7 - Avec type de retour bon" ((tp_fdefn env0 testExo7) = okExo7);;
resTest "Exercice 7 - Avec type de retour mauvais" ((tp_fdefn env0 testExo7) <> koExo7);;
resTest "Exercice 7 - Avec levé d'exception" (try (tp_fdefn env0 exExo7) = (tp_fdefn env0 exExo7) with
	FonctionMalTypee -> true | _ -> false);;




(* ****************** *)
(* *** Exercice 8 *** *)
(* ****************** *)

let testExo8 = Prog([Vardecl (IntT, "n"); Vardecl (BoolT, "b")], [(Fundefn (
	Fundecl (IntT, "f", [Vardecl (IntT, "n"); Vardecl (BoolT, "b")]),
	[Vardecl (IntT, "n"); Vardecl (BoolT, "b")],
	Seq (Skip, Return (Const (0, IntV 1)))))]);;

let okExo8 = Prog([], [(Fundefn (
	Fundecl (IntT, "f", [Vardecl (IntT, "n"); Vardecl (BoolT, "b")]),
	[Vardecl (IntT, "n"); Vardecl (BoolT, "b")],
	Seq (Skip, Return (Const (IntT, IntV 1)))))]);;

let koExo8 = Prog([Vardecl (IntT, "n"); Vardecl (BoolT, "b")], [(Fundefn (
	Fundecl (IntT, "f", [Vardecl (IntT, "n"); Vardecl (BoolT, "b")]),
	[Vardecl (IntT, "n"); Vardecl (BoolT, "b")],
	Seq (Skip, Return (Const (IntT, IntV 1)))))]);;

let exExo8 = Prog([Vardecl (IntT, "n"); Vardecl (BoolT, "b")], [(Fundefn (
	Fundecl (IntT, "f", [Vardecl (IntT, "n"); Vardecl (BoolT, "b")]),
	[Vardecl (IntT, "n"); Vardecl (BoolT, "b")],
	Seq (Skip, Return (Const (0, BoolV true)))))]);;

resTest "Exercice 8 - Avec programme bon" ((tp_prog testExo8) = okExo8);;
resTest "Exercice 8 - Avec programme mauvais" ((tp_prog testExo8) <> koExo8);;
resTest "Exercice 8 - Avec fonction mal typée" (try (tp_prog exExo8) = (tp_prog exExo8) with
	ProgrammeMauvais -> true | _ -> false);;














