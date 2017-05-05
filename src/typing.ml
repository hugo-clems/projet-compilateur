(* Typechecking of source programs *)

open Lang;;
open Analyses;;


(* Environments *)
type environment = {localvar: (vname * tp) list; globalvar: (vname * tp) list;
					returntp: tp; funbind: fundecl list};;








(* ************************************************* *)
(* *** 2. Typage et compilation pour expressions *** *)
(* ************************************************* *)

(* ****** 2.1 - Typage d’expressions simples ******* *)
(* ********** 2.2 - Typage avec fonctions ********** *)
(* Corresponds aux exercices 1 & 2 *)




(* *** Gestion des erreurs *** *)

(* Si la variable/fonction recherchée n'est pas présente dans l'environnement de recherche *)
exception PasDansEnv;;


(* Si il y a une erreur de types (dans les opérations ou IfThenElse) *)
exception ErreurTypes;;


(* Si les arguments passés dans CallE ne correspondent pas à la fonction utilisée *)
exception MauvaisArguments;;




(* *** Fonctions auxiliaires *** *)

(**
 * Transforme le type "value" en type "tp"
 * @param le type "value"
 * @return le type "tp"
 *)
let valueTOtp = function
	| BoolV _ -> BoolT
	| IntV _ -> IntT
	| VoidV -> VoidT;;


(**
 * Vérifie que deux expressions ont le même type
 * @param les expressions ('a expr)
 * @return true si les expressions ont le même types, false sinon
 *)
let memeType = function exp1 -> function exp2 -> (Lang.tp_of_expr exp1) == (Lang.tp_of_expr exp2);;


(**
 * Vérifie si une expression est bien d'un type donné
 * @param exp1 - l'expression ('a expr)
 * @param leType - le type éventuel de l'expression
 * @return true si l'expression est bien de ce type, false sinon
 *)
let estCeType = function exp1 -> function leType -> (Lang.tp_of_expr exp1) == leType;;


(* Accesseurs sur fundecl (en complèment de celui présent dans lang) *)
let type_of_fundecl (Fundecl (t, fn, pds)) = t;;
let name_of_fundecl (Fundecl (t, fn, pds)) = fn;;


(**
 * Recherche une fonction dans l'environnement
 * @param nomR - le nom de la fonction recherchée
 * @param l'environnement de recherche
 * @return la fonction recherché
 * @throw PasDansEnv - si la fonction n'est pas présente dans l'environnement
 *)
let rec parcoursFct = function nomR -> function
	| [] -> raise PasDansEnv
	| ((Fundecl (a,leNom,c))::r) -> if leNom = nomR then Fundecl (a,leNom,c) else (parcoursFct nomR r);;


(**
 * Vérifie que les types des arguments (CallE) sont compatibles
 * avec les types des paramètres (fundecl)
 * @param (arguments de CallE, paramètres de fundecl)
 * @return true si les types sons compatibles, false sinon
 *)
let rec verifCallE = function
	| ([],_) -> true
	| (_,[]) -> false
	| ((arg::resteA),(param::resteP)) -> estCeType arg (Lang.tp_of_vardecl param) && verifCallE(resteA,resteP);;


(**
 * Donne le type d'une opération
 * Op. arithmétique : IntT * IntT -> IntT
 * Op. comparaison : 'a * 'a -> BoolT
 * Op. logique : BoolT * BoolT -> BoolT
 * @param e1 - expression de l'opération
 * @param e2 - expression de l'opération
 * @param type de l'opération
 * @return le type de l'opération (tp)
 * @throw ErreurTypes - si les expressions n'ont pas des types compatibles pour l'opération
 *)
let typeOperations = function e1 -> function e2 -> function
	| BArith _ -> if estCeType e1 IntT && estCeType e2 IntT then IntT else raise ErreurTypes
	| BCompar _ -> if memeType e1 e2 then BoolT else raise ErreurTypes
	| BLogic _ -> if estCeType e1 BoolT && estCeType e2 BoolT then BoolT else raise ErreurTypes;;




(* *** Fonction principale *** *)

(**
 * Vérifie et annote (type) une expression
 * @param env - l'environnement actuel
 * @param l'expression à vérifier
 * @return l'expression typée
 *)
let rec tp_expr = function env -> function
	| Const ((_ : int), cvaleur) -> (Const ((valueTOtp cvaleur), cvaleur))
	| VarE ((_ : int), Var (varBinding, vVname)) ->
		(VarE ((List.assoc vVname env.localvar), Var (varBinding, vVname)))
	| BinOp ((_ : int), bbinop, bexpr1, bexpr2) ->
		let e1 = (tp_expr env bexpr1) and e2 = (tp_expr env bexpr2) in
			(BinOp ((typeOperations e1 e2 bbinop), bbinop, e1, e2))
	| IfThenElse ((_ : int), iexpr1, iexpr2, iexpr3) ->
		let e1 = (tp_expr env iexpr1) and e2 = (tp_expr env iexpr2) and e3 = (tp_expr env iexpr3) in
			if estCeType e1 BoolT && memeType e2 e3
			then IfThenElse ((Lang.tp_of_expr e2), e1, e2, e3) else raise ErreurTypes
	| CallE ((_ : int), nom, args) -> 
		let f = parcoursFct nom env.funbind and arguments = List.map (tp_expr env) args in
			if verifCallE(arguments, Lang.params_of_fundecl f)
			then CallE (type_of_fundecl f, nom, arguments) else raise MauvaisArguments;;








(* ************************************************ *)
(* *** 3. Typage et compilation pour programmes *** *)
(* ************************************************ *)

(* ****************** 3.1 Typage ****************** *)
(* Corresponds aux exercices 6, 7 & 8 *)




(* *** Gestion des erreurs *** *)

exception FonctionMalTypee;;
exception MotsInterdits;;




(**
 * Type un statement (stmt)
 * @param env - l'environnement de recherche
 * @param le statement à typer
 * @return le statement typé
 *)
let rec tp_stmt = function env -> function
	| Skip -> Skip
	| Assign (aType, aVar, aExpr) -> Assign(VoidT, aVar, (tp_expr env aExpr))
	| Seq (s1, s2) -> Seq((tp_stmt env s1), (tp_stmt env s2))
	| Cond (cExpr, c1, c2) -> Cond((tp_expr env cExpr), (tp_stmt env c1), (tp_stmt env c2))
	| While (wExpr, wStmt) -> While((tp_expr env wExpr), (tp_stmt env wStmt))
	| CallC (cName, exprList) -> CallC(cName, (List.map (tp_expr env) exprList))
	| Return rExpr -> Return(tp_expr env rExpr);;


(* Liste des mots interdits *)
let forbiddenKeywords = [
	"auto";
	"break";
	"case"; "char"; "const"; "continue";
    "default"; "do"; "double";
    "else"; "enum"; "extern";
    "float"; "for";
    "goto";
    "if"; "int";
    "long";
    "register"; "return";
    "short"; "signed"; "sizeof"; "static"; "struct"; "switch";
    "typedef";
    "union"; "unsigned";
    "void"; "volatile";
    "while"
];;




(* *** Fonctions auxiliaires *** *)

(**
 * Vérifie que le statement renvoie bien le bon type
 * (c'est-à-dire qu'il est du même type que la fonction du Fundefn)
 * @param tpF - le type de la fonction
 * @param le statement à vérifier
 * @return true si le type est bon, false sinon
 *)
let rec fdefnVerifTypeF = function tpF -> function
	| Return rExpr -> tpF == (Lang.tp_of_expr rExpr)
	| Seq (s1, s2) -> (fdefnVerifTypeF tpF s1) || (fdefnVerifTypeF tpF s2)
	| Cond (cExpr, c1, c2) -> (fdefnVerifTypeF tpF c1) || (fdefnVerifTypeF tpF c2)
	| While (wExpr, wStmt) -> (fdefnVerifTypeF tpF wStmt)
	| _ -> false;;


(**
 * Prépare une liste de Vardecl pour être inséré dans l'environnement
 * @param la liste de Vardecl
 * @return la liste des variables (nom, type)
 *)
let rec prepareVars = function
	| [] -> []
	| ((Vardecl (vTp, vNom))::reste) -> (vNom, vTp)::(prepareVars reste);;


(**
 * Mets à jour l'environnement
 * @param env - l'environnement à mettre à jour
 * @param vars - les variables à ajouter dans l'environnement
 * @param rType - le type de retour
 * @return le nouvel environnement
 *)
let majEnv = function env -> function vars -> function rType ->
	{localvar = vars @ (env.localvar); globalvar = env.globalvar; returntp = rType; funbind = env.funbind};;


(**
 * Vérifie que les fonctions n'utilisent pas de mots interdits
 * @param lesMotsInterdits - la liste des mots interdits
 * @param les fonctions à vérifier
 * @return boolean
 *)
let rec pasDeMotsInterdits = function lesMotsInterdits -> function 
	| [] -> true
	| (Vardecl (_, nom)::reste) -> if (List.mem nom lesMotsInterdits)
								   then raise MotsInterdits
								   else pasDeMotsInterdits lesMotsInterdits reste;;




(* *** Fonctions principales *** *)

(**
 * Type la fonction et vérifie que la fonction est bien définie
 * @param env - l'environnement à mettre à jour
 * @param Fundefn (...) - la fonction à vérifier et ajouter
 *			fonction - type, nom & paramètres de la fonction
 *			variables - variables de la fonction
 *			fStmt - corps de la fonction
 * @return la fonction typée
 * @throw FonctionMalTypee - si la fonction est mal typée
 * @throw MotsInterdits - si la fonction contient des mots interdits forbiddenKeywords
 *)
let tp_fdefn = function env -> function Fundefn (Fundecl (fTp, fnom, fVardecl) as fonction, variables, fStmt) ->
	let vars = (prepareVars (fVardecl @ variables)) in let env2 = (majEnv env vars fTp) in
		let newSmtp = (tp_stmt env2 fStmt) in
			if (pasDeMotsInterdits forbiddenKeywords fVardecl)
				&& (Analyses.stmt_returns newSmtp) && (fdefnVerifTypeF fTp newSmtp)
			then Fundefn (fonction, variables, newSmtp)
			else raise FonctionMalTypee;;


(**
 * Vérifie un programme
 * Créer un environnement initial avec les variables globales,
 * et vérifie les définitions de fonctions avec cette environnement.
 * @param varGlob - liste des variables globales
 * @param fundefListe - liste des fonctions
 * @return le programme avec les fonctions vérifiées
 *)
let tp_prog = function Prog (varGlob, fundefListe) ->
	let env = {localvar = []; globalvar = (prepareVars varGlob); returntp = VoidT;
				funbind = (List.map (function Fundefn(decl, _, _) -> decl) fundefListe)}
	in Prog([], List.map (tp_fdefn env) fundefListe);;




(* *** tp_prog original *** *)

(*let tp_prog (Prog (gvds, fdfs)) =
	Prog([],
		[Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [], Skip)])
;;*)






