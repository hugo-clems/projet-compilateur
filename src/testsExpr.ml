(* ********************* *)
(* Tests de l'exercice 5 *)
(* ********************* *)

(* Ressources nécéssaires *)
#use "use.ml";;
open Lang;;
open Analyses;;
open Instrs;;
open Typing;;
open Gen;;
open Print_instr;;

let env1 = {localvar = [("k", IntT); ("n", IntT)]; globalvar = []; returntp = VoidT;
			funbind = [Fundecl (IntT, "f", [Vardecl (IntT, "n"); Vardecl (BoolT, "b")])]};;

let rec affichage = function
	| [] -> []
	| (a::q) -> (pr_instr a)::(affichage q);;

let rec test = function
	| [] -> []
	| (a::q) -> (affichage (gen_expr ["n"] (tp_expr env1 a)))::(test q);;



(* Expressions à tester *)
let expressions = [
	BinOp (0, BArith BAadd, Const (0, IntV 3), Const (0, IntV 4));
	BinOp (0, BArith BAsub, VarE (0, Var (Local, "n")), Const (0, IntV 4));
	BinOp (0, BArith BAmul, Const (0, IntV 3), VarE (0, Var (Local, "n")));
	BinOp (0, BArith BAdiv, Const (0, IntV 10), Const (0, IntV 2));
	BinOp (0, BArith BAmod, Const (0, IntV 10), Const (0, IntV 2));
];;




(* Le test *)
(* Pour chaque expression : *)
(* Etape 1 : tp_expr envX exprX *)
(* Etape 2 : gen_expr [] etape1 *)
(* Etape 3 : test etape2   *)

test expressions;;

(*
	On obtient :
	[["sipush 3"; "sipush 4"; "iadd"];
	 ["iload 0"; "sipush 4"; "isub"];
 	 ["sipush 3"; "iload 0"; "imul"];
 	 ["sipush 10"; "sipush 2"; "idiv"];
 	 ["sipush 10"; "sipush 2"; "irem"]]
*)



(*
	Résultat après copie dans Even.j et exécution avec Java :
	
	java Wrapper 0			result: 7			(* 3+4  *)
	java Wrapper 5			result: 1			(* 5-4  *)
	java Wrapper 4			result: 12			(* 3*4  *)
	java Wrapper 0			result: 5			(* 10/2 *)
	java Wrapper 0			result: 0			(* 10%2  *)
*)






