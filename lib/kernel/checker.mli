open Term

(** Typing environment *)
type type_env

val new_env : type_env
(** Empty environment *)

(** Error report *)
type error =
  | Bad_type of ptype
  | Not_typable
  | Overwrite

val type_check : term -> type_env -> ptype option
(** Perform type checking 
    @param t  The term to type check
    @param e  The current typing environment *)

val extend_env : string -> ptype -> term -> type_env -> (type_env, error) result
(** Try to extend current env with a new proposition
    @param n  The name of the new proposition
    @param p  The proposition
    @param t  The term proposed as a proof
    @param e  The current typing environment *)

val extend_by_axiom : string -> ptype -> type_env -> (type_env, error) result
(** Try to extend current env with a new axiom
    @param n  The name of the new axiom
    @param p  The new axiom 
    @param e  The current typing environment *)