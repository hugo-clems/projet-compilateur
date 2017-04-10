(* Compilation functions *)

open Lang
open Analyses
open Instrs








(* ************************************************* *)
(* *** 2. Typage et compilation pour expressions *** *)
(* ************************************************* *)

(* *** 2.3 - Génération de code pour expressions *** *)
(* Corresponds aux exercices 3, 4 & 5 *)




(* *** Gestion des erreurs *** *)

(* Si un élément ne se trouve pas dans la liste *)
exception PasDansLaliste;;


(* Supprime les warnings lors de la compilation, code temporaire *)
exception CodeAVenirDansLaPartieII;;




(* *** Fonction auxiliaire *** *)


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
 * @param l'expression à traduire
 * @return l'instruction traduite
 * @throw CodeAVenirDansLaPartieII - TEMPORAIRE
 *)
let rec gen_expr = function variables -> function
	| Const (cType, cValeur) -> [Loadc (cType, cValeur)]
	| VarE (vType, Var (varBinding, vVname)) -> [Loadv (vType, (position vVname variables))]
	| BinOp (bType, bBinop, bExpr1, bExpr2) ->
		let e1 = (gen_expr variables bExpr1) and e2 = (gen_expr variables bExpr2) in
			e1 @ e2 @ [Bininst (bType, bBinop)]
	| IfThenElse (iType, iexpr1, iexpr2, iexpr3) -> raise CodeAVenirDansLaPartieII
	| CallE (ceType, ceNom, ceArgs) -> raise CodeAVenirDansLaPartieII;;








(* ************************************************************ *)
(* **** Compilation of methods / programs                  **** *)
(* ************************************************************ *)

let gen_prog (Prog (gvds, fdfs)) = 
  JVMProg ([], 
           [Methdefn (Methdecl (IntT, "even", [IntT]),
                      Methinfo (3, 1),
                      [Loadc (IntT, IntV 0); ReturnI IntT])])



