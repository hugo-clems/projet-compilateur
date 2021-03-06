(* Compilation functions *)

open Lang
open Analyses
open Instrs




(* ************************************************* *)
(* *** 2. Typage et compilation pour expressions *** *)
(* ************************************************* *)

(* *** 2.3 - Génération de code pour expressions *** *)
(* Corresponds aux exercices 3, 4, 5 & 9 *)




(* *** Gestion des erreurs *** *)

(* Si un élément ne se trouve pas dans la liste *)
exception PasDansLaliste;;




(* *** Fonctions auxiliaires *** *)


(**
 * Détermine la position d'un élément dans une liste
 * @param elt - l'élément recherché
 * @param la liste où l'on cherche
 * @return l'indice de position de l'élément (à partir de 0)
 * @throw PasDansLaliste - si l'élément ne se trouve pas dans la liste
 *)
let rec position = function elt -> function
	| [] -> raise PasDansLaliste
	| (a::reste) -> if a = elt then 0 else 1 + position elt reste;;

(**
 * Transforme un BoolT en IntT
 * @param le type à transformer
 * @return le type transformé
 *)
let boolToInt = function
	| BoolT -> IntT
	| leType -> leType;;


(* *** Fonction principale *** *)

(**
 * Traduit une expression en instruction
 * @param variables - liste des variables de la fonction actuelle
 * @param chemin - le chemin actuel pour les labels
 * @param l'expression à traduire
 * @return l'instruction traduite
 *)
let rec gen_expr = function variables -> function chemin -> function
	| VarE (vType, Var (varBinding, vVname)) -> [Loadv (vType, (position vVname variables))]
	| (Const (IntT,  IntV  i)) -> [Loadc (IntT, IntV i)]
	| (Const (BoolT, BoolV b)) -> let i = if b then 1 else 0 in [Loadc (IntT, IntV i)]
	| (Const _) -> failwith "Const can't be void..."
	| (BinOp (bType, bBinop, b1, b2)) -> 
		let leGen = (gen_expr variables) in let e1 = leGen chemin b1 and e2 = leGen chemin b2 
		in begin (match bBinop with 
			| BArith _ | BLogic _ -> e1 @ e2 @ [Bininst (IntT, bBinop)]
			| BCompar cmpOperator ->
				let lTrue = chemin @ [0] and lFin = chemin @ [1]
				in e1 @ e2 @ If (cmpOperator, lTrue)::Loadc (IntT, IntV 0)::Goto lFin 
					::Label lTrue::Loadc (IntT, IntV 1)::[Label lFin]) end
	| IfThenElse (iType, ie, i1, i2) -> let lFalse = chemin @ [0] and lFin = chemin @ [2] in
		(gen_expr variables lFin ie) @ [Loadc (IntT, (IntV 0))] @ [If (BCeq, lFalse)] @
		(gen_expr variables (chemin @ [1]) i1) @ [Goto lFin] @
		[Label lFalse] @ (gen_expr variables lFalse i2) @ [Label lFin]
	| CallE (ceType, ceNom, ceArgs) ->
		let rec genCallE = fun listeInstr listeType -> function
			| [] -> (listeInstr, listeType)
			| (elt::reste) -> let instrElt = (gen_expr variables chemin elt) and tpElt =(Lang.tp_of_expr elt) in
				genCallE (listeInstr @ instrElt) (listeType @ [tpElt]) reste
		in let (listeInstr, listeType) = (genCallE [] [] ceArgs) in
			listeInstr @ [Invoke (ceType, ceNom, listeType)] @ [ReturnI ceType];;








(* ************************************************* *)
(* *** 3. Typage et compilation pour programmes  *** *)
(* ************************************************* *)

(* *** 3.2 - Génération de code *** *)
(* Corresponds aux exercices 10 & 11 *)


(**
 * Traduit un statement en instruction
 * @param variables - liste des variables de la fonction actuelle
 * @param chemin - le chemin actuel pour les labels
 * @param le statement à traduire
 * @return l'instruction traduite
 *)
let rec gen_stmt = fun variables chemin -> function
	| Skip -> [Nop]
	| Assign (tp, Var(binding, vname), exp) ->
	let instrsList = gen_expr variables chemin exp in
		if binding = Local then
			let pos = position vname variables 
			in instrsList @ [Storev (IntT, pos)]
		else instrsList @ [Putstatic (IntT, vname)]
	| Seq (s1, s2) -> (gen_stmt variables chemin s1) @ (gen_stmt variables chemin s2)
	| Cond (cExpr, c1, c2) -> let lFalse = chemin @ [0] and lFin = chemin @ [2] in
		(gen_expr variables lFin cExpr) @ [Loadc (IntT, (IntV 0))] @ [If (BCeq, lFalse)] @
		(gen_stmt variables (chemin @ [1]) c1) @ [Goto lFin] @
		[Label lFalse] @ (gen_stmt variables lFalse c2) @ [Label lFin]
	| While (wExpr, wStmt) -> let lFalse = chemin @ [0] and lWhile = chemin @ [1] in
		[Label lWhile] @ (gen_expr variables (chemin @ [2]) wExpr) @ [Loadc (IntT, (IntV 0))] @
		[If (BCeq, lFalse)] @ (gen_stmt variables lWhile wStmt) @ [Goto lWhile] @
		[Label lFalse] @ [Nop]
	| CallC (cName, exprList) ->
		let rec genCallC = fun listeInstr listeType -> function
			| [] -> (listeInstr, listeType)
			| (elt::reste) -> let instrElt = (gen_expr variables chemin elt) and tpElt =(Lang.tp_of_expr elt) in
				genCallC (listeInstr @ instrElt) (listeType @ [tpElt]) reste
		in let (listeInstr, listeType) = (genCallC [] [] exprList) in
			listeInstr @ [Invoke (VoidT, cName, listeType)] @ [ReturnI VoidT]
	| Return rExpr -> [ReturnI (boolToInt (Lang.tp_of_expr rExpr))];;


	
	
	



(**
 * Traduit toute une définition de fonction (l’en-tête et le corps)
 * @param Fundefn (...) - la fonction à vérifier et ajouter
 *			fonction - type, nom & paramètres de la fonction
 *			variables - variables locales de la fonction
 *			fStmt - corps de la fonction
 *)
let gen_fundefn = function Fundefn (Fundecl (fTp, fNom, fParams), variables, fStmt) ->
	Methdefn (
		Methdecl (boolToInt fTp, fNom, (List.map tp_of_vardecl fParams)),
		Methinfo ((1 + (Analyses.stack_depth_c fStmt)), (List.length variables)),
		(gen_stmt (List.map name_of_vardecl (fParams @ variables)) [] fStmt)
	);;




(* ************************************************************ *)
(* **** Compilation of methods / programs                  **** *)
(* ************************************************************ *)

let gen_prog = function Prog (varGlob, fundefListe) -> JVMProg ([], List.map gen_fundefn fundefListe);;


(* Ancien gen_prog *)
(*let gen_prog (Prog (gvds, fdfs)) = 
  JVMProg ([], 
           [Methdefn (Methdecl (IntT, "even", [IntT]),
                      Methinfo (3, 1),
                      [Loadc (IntT, IntV 0); ReturnI IntT])])*)

