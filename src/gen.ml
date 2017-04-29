(* Compilation functions *)

open Lang
open Analyses
open Instrs

let tp_of_expr = function
    Const (t, _) -> t
  | VarE (t, _) -> t
  | BinOp (t, _, _, _) -> t
  | IfThenElse (t, _, _, _) -> t
  | CallE (t, _, _) -> t








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




(* *** Fonction principale *** *)

(**
 * Traduit une expression en instruction
 * @param variables - liste des variables de la fonction actuelle
 * @param chemin - le chemin actuel pour les labels
 * @param l'expression à traduire
 * @return l'instruction traduite
 *)
let rec gen_expr = function variables -> function chemin -> function
	| Const (cType, cValeur) -> [Loadc (cType, cValeur)]
	| VarE (vType, Var (varBinding, vVname)) -> [Loadv (vType, (position vVname variables))]
	| BinOp (bType, bBinop, bExpr1, bExpr2) ->
		let e1 = (gen_expr variables chemin bExpr1) and e2 = (gen_expr variables chemin bExpr2) in
			e1 @ e2 @ [Bininst (bType, bBinop)]
	| IfThenElse (iType, ie, i1, i2) -> let lFalse = chemin @ [0] and lFin = chemin @ [2] in
		(gen_expr variables lFin ie) @ [Loadc (IntT, (IntV 0))] @ [If (BCeq, lFalse)] @
		(gen_expr variables (chemin @ [1]) i1) @ [Goto lFin] @
		[Label lFalse] @ (gen_expr variables lFalse i2) @ [Label lFin]
	| CallE (ceType, ceNom, ceArgs) ->
		let rec genCallE = fun listeInstr listeType -> function
			| [] -> (listeInstr, listeType)
			| (elt::reste) -> let instrElt = (gen_expr variables chemin elt) and tpElt = (tp_of_expr elt) in
				genCallE (listeInstr @ instrElt) (listeType @ [tpElt]) reste
		in let (listeInstr, listeType) = (genCallE [] [] ceArgs) in
			listeInstr @ [Invoke (ceType, ceNom, listeType)] @ [ReturnI ceType];;








(* ************************************************* *)
(* *** 3. Typage et compilation pour programmes  *** *)
(* ************************************************* *)

(* *** 3.2 - Génération de code *** *)
(* Corresponds aux exercices 10 & 11 *)

(* TODO *)


(* ************************************************************ *)
(* **** Compilation of methods / programs                  **** *)
(* ************************************************************ *)

let gen_prog (Prog (gvds, fdfs)) = 
  JVMProg ([], 
           [Methdefn (Methdecl (IntT, "even", [IntT]),
                      Methinfo (3, 1),
                      [Loadc (IntT, IntV 0); ReturnI IntT])])



