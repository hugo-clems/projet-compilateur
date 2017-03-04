(* Compilation functions *)

open Lang
open Analyses
open Instrs

(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)



(* ************************************************************ *)
(* **** Compilation of methods / programs                  **** *)
(* ************************************************************ *)

let gen_prog (Prog (gvds, fdfs)) = 
  JVMProg ([], 
           [Methdefn (Methdecl (IntT, "even", [IntT]),
                      Methinfo (3, 1),
                      [Loadc (IntT, IntV 0); ReturnI IntT])])


(* Si un élément ne se trouve pas dans la liste *)
exception PasDansLaliste;;


(* Détermine la position d'un élément dans la liste (à partir de 0) *)
let rec position = function elt -> function
	| [] -> raise PasDansLaliste
	| (a::reste) -> if a = elt then 0 else 1 + position elt reste;;