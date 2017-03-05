(* Compilation functions *)

open Lang
open Analyses
open Instrs

(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)


(* Si un élément ne se trouve pas dans la liste *)
exception PasDansLaliste;;


(* Détermine la position d'un élément dans la liste (à partir de 0) *)
let rec position = function elt -> function
	| [] -> raise PasDansLaliste
	| (a::reste) -> if a = elt then 0 else 1 + position elt reste;;


exception CodeAVenirDansLaPartieII;;

(* Transforme l'expression en ByteCode *)
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



