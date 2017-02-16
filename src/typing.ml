(* Typechecking of source programs *)

open Lang
open Analyses

(* Environments *)

type environment = 
    {localvar: (vname * tp) list; 
     globalvar: (vname * tp) list; 
     returntp: tp;
     funbind: fundecl list}


(* TODO: put your definitions here *)
let tp_prog (Prog (gvds, fdfs)) =
  Prog([],
       [Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [], Skip)])
;;

(* Besoin ? Ici ? *)
type tp = BoolT | IntT | VoidT;

(* Vérifie expression dans l'environnement et l’annote avec des types *)
let tp_expr = function env -> function exp ->
(* A SUIVRE *)