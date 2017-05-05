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

exception ExpressionIndefinie;;


module StringSet = 
  Set.Make
    (struct type t = string 
	    let compare = Pervasives.compare 
     end)


(**
 * Vérifie que toutes les variables de e sont présentes dans vs.
 * @param vs - ensemble de variables qui ont certainement une valeur
 * @param e - expression dont on vérifie toutes les variables
 * @return true si toutes les variables de e sont présentes dans vs
 *)
let rec defassign_e = function vs -> function
	| Const (_, _) -> true
	| VarE (_, Var (_, vVname)) -> StringSet.mem vVname vs (* (StringSet.of_list vs) *)
	| BinOp (_, _, bExpr1, bExpr2) -> (defassign_e vs bExpr1) && (defassign_e vs bExpr2)
	| IfThenElse (_, ie, i1, i2) -> (defassign_e vs ie) && (defassign_e vs i1) && (defassign_e vs i2)
	| CallE (_, _, ceArgs) ->
		let rec defCallE = function
			| (expr::reste)-> (defassign_e vs expr) && (defCallE reste)
			| _ -> true
		in defCallE ceArgs;;


(**
 * Vérifie que toutes les variables de i sont présentes dans vs.
 * @param vs - ensemble de variables qui ont certainement une valeur
 * @param i - instruction dont on vérifie toutes les variables
 * @return vs
 * @throw ExpressionIndefinie - si une des sous-expressions renvoie false
 *)
let rec defassign_c = function vs -> function
	| Skip -> vs
	| Assign (aType, Var (_, avName), aExpr) ->
		if (defassign_e vs aExpr)
		then StringSet.add avName vs
		else raise ExpressionIndefinie
	| Seq (s1, s2) ->
		let vs = defassign_c vs s1 in defassign_c vs s2
	| Cond (cExpr, c1, c2) ->
		if (defassign_e vs cExpr)
		then let sA = defassign_c vs c1 in let sB = defassign_c vs c2 in StringSet.inter sA sB
		else raise ExpressionIndefinie
	| While (wExpr, wStmt) ->
		if (defassign_e vs wExpr)
		then let _ = defassign_c vs wStmt in vs
		else raise ExpressionIndefinie
	| CallC (cName, exprList) ->
		let defCallC = List.map (defassign_e vs) exprList in
			if List.exists not defCallC
			then raise ExpressionIndefinie
			else vs
	| Return rExpr -> if (defassign_e vs rExpr) then vs else raise ExpressionIndefinie;;








