(* Typechecking of source programs *)

open Lang
open Analyses

(* Environments *)

type environment = 
    {localvar: (vname * tp) list; 
     globalvar: (vname * tp) list; 
     returntp: tp;
     funbind: fundecl list};;

(* Pour une raison inconnu, il est nécéssaires de redéfinir ces fonctions présentes dans lang.ml *)
let tp_of_vardecl (Vardecl (t, _)) = t;;
let tp_of_expr = function
	| Const (t, _) -> t
	| VarE (t, _) -> t
	| BinOp (t, _, _, _) -> t
	| IfThenElse (t, _, _, _) -> t
	| CallE (t, _, _) -> t;;
let params_of_fundecl (Fundecl (t, fn, pds)) = pds;;

(* TODO: put your definitions here *)
let tp_prog (Prog (gvds, fdfs)) =
	Prog([],
		[Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [], Skip)])
;;




(* **************** *)
(* *** Mon code *** *)
(* **************** *)

(* Si la variable/fonction recherchée n'est pas présente dans l'environement de recherche *)
exception PasDansEnv;;

(* Si il y a une erreur de types (dans les opérations ou IfThenElse) *)
exception ErreurTypes;;

(* Si les arguments passés dans CallE ne correspondent pas à la fonction utilisée *)
exception MauvaisArguments;;


(* Applique une fonction à tous les éléments de la liste *)
let listmap = function f -> function liste ->
	let rec aux = function
		| [] -> []
		| (a::q) -> (f a)::(aux q)
	in aux(liste);;


(* Transforme le type value en type tp *)
let valueTOtp = function
	| BoolV _ -> BoolT
	| IntV _ -> IntT
	| VoidV -> VoidT;;


(* Vérifie si 2 expressions ont le même type *)
let memeType = function exp1 -> function exp2 -> (tp_of_expr exp1) == (tp_of_expr exp2);;

(* Vérifie si une expressions est bien d'un type donné *)
let estCeType = function exp1 -> function leType -> (tp_of_expr exp1) == leType;;


(* Parcours l'environement à la recherche de l'élément n, renvoie son type associé *)
let rec parcoursEnv = function n -> function
	| [] -> raise PasDansEnv
	| ((a,b)::r) -> if a = n then b else (parcoursEnv n r);;


(* Accesseurs sur fundecl (en complèment de celui présent dans lang *)
let type_of_fundecl (Fundecl (t, fn, pds)) = t;;
let name_of_fundecl (Fundecl (t, fn, pds)) = fn;;


(* Parcours la liste des fonctions présente dans l'environement *)
let rec parcoursFct = function nomR -> function
	| [] -> raise PasDansEnv
	| ((Fundecl (a,leNom,c))::r) -> if leNom = nomR then Fundecl (a,leNom,c) else (parcoursFct nomR r);;


(* Vérifie que les types des arguments (CallE) sont compatibles
 * avec les types des paramètres(fundecl)
 *)
let rec verifCallE = function
	| ([],_) -> true
	| (_,[]) -> false
	| ((arg::resteA),(param::resteP)) -> estCeType arg (tp_of_vardecl param) && verifCallE(resteA,resteP);;


(* Donne le type d'une opération
 * Op. arithmétique : IntT * IntT -> IntT
 * Op. comparaison : 'a * 'a -> BoolT
 * Op. logique : BoolT * BoolT -> BoolT
 *)
let typeOperations = function e1 -> function e2 -> function
	| BArith _ -> if estCeType e1 IntT && estCeType e2 IntT then IntT else raise ErreurTypes
	| BCompar _ -> if memeType e1 e2 then BoolT else raise ErreurTypes
	| BLogic _ -> if estCeType e1 BoolT && estCeType e2 BoolT then BoolT else raise ErreurTypes;;


(* Fonction principale : Vérifie l'expression et l'annote avec des types *)
let rec tp_expr = function env -> function
	| Const ((_ : int), cvaleur) -> (Const ((valueTOtp cvaleur), cvaleur))
	| VarE ((_ : int), Var (varBinding, vVname)) ->
		(VarE ((parcoursEnv vVname env.localvar), Var (varBinding, vVname)))
	| BinOp ((_ : int), bbinop, bexpr1, bexpr2) ->
		let e1 = (tp_expr env bexpr1) and e2 = (tp_expr env bexpr2) in
			(BinOp ((typeOperations e1 e2 bbinop), bbinop, e1, e2))
	| IfThenElse ((_ : int), iexpr1, iexpr2, iexpr3) ->
		let e1 = (tp_expr env iexpr1) and e2 = (tp_expr env iexpr2) and e3 = (tp_expr env iexpr3) in
			if estCeType e1 BoolT && memeType e2 e3
			then IfThenElse ((tp_of_expr e2), e1, e2, e3) else raise ErreurTypes
	| CallE ((_ : int), nom, args) -> 
		let f = parcoursFct nom env.funbind and arguments = listmap (tp_expr env) args in
			if verifCallE(arguments, params_of_fundecl f)
			then CallE (type_of_fundecl f, nom, arguments) else raise MauvaisArguments;;






