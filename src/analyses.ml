(* Analyses of source language statements:
 - predicate: 'statement returns'
 - depth of operand stack for expression evaluation
 - definite assignment
*)

open Lang

(* ************************************************************ *)
(* ****  Statement returns                                 **** *)
(* ************************************************************ *)

(**
 * Vérifie si une fonction termine chaque branche d’exécution correctement
 * (c'est-à-dire avec un return ou un return expression)
 * @param le stmt à vérifier
 * @return true si le stmt finis bien par un return, false sinon
 *)
let rec stmt_returns = function
	| Skip -> false
	| Assign (aType, aVar, aExpr) -> false
	| Seq (s1, s2) -> stmt_returns(s1) || stmt_returns(s2)
	| Cond (cExpr, c1, c2) -> stmt_returns(c1) || stmt_returns(c2)
	| While (wExpr, wStmt) -> stmt_returns(wStmt)
	| CallC (cName, exprList) -> true
	| Return rExpr -> true;;



(* ************************************************************ *)
(* ****  Stack depth                                       **** *)
(* ************************************************************ *)

(**
 * Calcul la somme des élèments d'une liste
 * @param la liste
 * @return la somme de ses éléments
 *)
let rec listsum = function
	| [] -> 0
	| (elt::reste) -> elt + (listsum reste);;


(**
 * Calcul la taille de la pile d'opérandes pour les expressions
 * @param l'expression
 * @return la taille de la pile d'opérandes nécéssaires
 *)
let rec stack_depth_e = function
	| Const (cType, cValeur) -> 1
	| VarE (vType, Var (varBinding, vVname)) -> 1
	| BinOp (bType, bBinop, bExpr1, bExpr2) -> max (stack_depth_e bExpr1) (stack_depth_e bExpr2)
	| IfThenElse (iType, ie, i1, i2) -> (stack_depth_e ie) + (max (stack_depth_e i1) (stack_depth_e i2))
	| CallE (ceType, ceNom, ceArgs) -> listsum (List.map stack_depth_e ceArgs);;


(**
 * Calcul la taille de la pile d'opérandes pour les instructions
 * @param l'instruction
 * @return la taille de la pile d'opérandes nécéssaires
 *)
let rec stack_depth_c = function
	| Skip -> 0
	| Assign (aType, aVar, aExpr) -> stack_depth_e aExpr
	| Seq (s1, s2) -> (stack_depth_c s1) + (stack_depth_c s2)
	| Cond (cExpr, c1, c2) -> (stack_depth_e cExpr) + (max (stack_depth_c c1) (stack_depth_c c2))
	| While (wExpr, wStmt) -> (stack_depth_e wExpr) + (stack_depth_c wStmt)
	| CallC (cName, exprList) -> listsum (List.map stack_depth_e exprList)
	| Return rExpr -> stack_depth_e rExpr;;



(* ************************************************************ *)
(* ****  Definite Assignment                               **** *)
(* ************************************************************ *)

module StringSet = 
  Set.Make
    (struct type t = string 
	    let compare = Pervasives.compare 
     end)

let rec defassign_e a = true

let rec defassign_c allvs a = a

