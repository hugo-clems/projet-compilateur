(* Typechecking of source programs *)

open Lang
open Analyses

(* Environments *)

type environment = 
    {localvar: (vname * tp) list; 
     globalvar: (vname * tp) list; 
     returntp: tp;
     funbind: fundecl list}

(* Pour une raison inconnu, il est nécéssaires de redéfinir ces fonctions présentes dans lang.ml *)
let tp_of_vardecl (Vardecl (t, _)) = t;;
let tp_of_expr = function
    Const (t, _) -> t
  | VarE (t, _) -> t
  | BinOp (t, _, _, _) -> t
  | IfThenElse (t, _, _, _) -> t
  | CallE (t, _, _) -> t;;

(* TODO: put your definitions here *)
let tp_prog (Prog (gvds, fdfs)) =
  Prog([],
       [Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [], Skip)])
;;




(* ****************** *)
(* *** Exercice 1 *** *)
(* ****************** *)

exception PasDansEnv;; (* Si la variable recherchée n'est pas présente dans l'environement de recherche *)
exception ErreurTypes;; (* Si il y a une erreur de types (dans les opérations ou IfThenElse) *)

(* Transforme le type value en type tp *)
let vTOt = function
	| BoolV _ -> BoolT
	| IntV _ -> IntT
	| VoidV -> VoidT;;

(* Parcours l'environement à la recherche de l'élément n, renvoie son type associé *)
let rec parcoursEnv = function n -> function
	| [] -> raise PasDansEnv
	| ((a,b)::r) -> if a = n then b else (parcoursEnv n r);;

(* Donne le type d'une opération
 * Op. arithmétique : IntT * IntT -> IntT
 * Op. comparaison : 'a * 'a -> BoolT
 * Op. logique : BoolT * BoolT -> BoolT
 *)
let typeOperations = function e1 -> function e2 -> function
	| BArith _ -> if tp_of_expr e1 == IntT && tp_of_expr e2 == IntT then IntT else raise ErreurTypes
	| BCompar _ -> if tp_of_expr e1 == tp_of_expr e2 then BoolT else raise ErreurTypes
	| BLogic _ -> if tp_of_expr e1 == BoolT && tp_of_expr e2 == BoolT then BoolT else raise ErreurTypes;;

(* Fonction principale : Vérifie l'expression et l'annote avec des types *)
let rec tp_expr = function env -> function
	| Const ((_ : int), cvaleur) -> (Const ((vTOt cvaleur), cvaleur))
	| VarE ((_ : int), Var (varBinding, vVname)) -> (VarE (
		(parcoursEnv vVname env.localvar)
		, Var (varBinding, vVname) ))
	| BinOp ((_ : int), bbinop, bexpr1, bexpr2) -> let e1 = (tp_expr env bexpr1) and e2 = (tp_expr env bexpr2) in
		(BinOp ((typeOperations e1 e2 bbinop), bbinop, e1, e2))
	| IfThenElse ((_ : int), iexpr1, iexpr2, iexpr3) ->
		let e1 = (tp_expr env iexpr1) and e2 = (tp_expr env iexpr2) and e3 = (tp_expr env iexpr3) in
			if (tp_of_expr e1) == BoolT && (tp_of_expr e2) == (tp_of_expr e3)
			then IfThenElse ((tp_of_expr e2), e1, e2, e3) else raise ErreurTypes;;


(* Tests *)
let env1 = {localvar = [("k", IntT); ("n", IntT)]; globalvar = []; returntp = VoidT; funbind = []};;
let exp1 = BinOp (0, BCompar BCeq, VarE (0, Var (Local, "n")),
									BinOp (0, BArith BAadd, VarE (0, Var (Local, "k")),
															Const (0, IntV 1)));;
let resultat1 = BinOp (BoolT, BCompar BCeq, VarE (IntT, Var (Local, "n")),
											BinOp (IntT, BArith BAadd, VarE (IntT, Var (Local, "k")),
																		Const (IntT, IntV 1)));;

print_string("***** On test avec l'exemple du sujet *****");;

print_string(if resultat1 = (tp_expr env1 exp1) then "Le test à réussi !" else "Le test à échoué !");;
