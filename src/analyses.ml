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



let rec stack_depth_e = 0

let rec stack_depth_c = 0



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

