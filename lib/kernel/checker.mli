open Term

type type_env
val new_env : type_env

type error =
  | Bad_type of ptype
  | Not_typable
  | Overwrite

val type_check : term -> type_env -> ptype option
val extend_env : string -> ptype -> term -> type_env -> (type_env, error) result
val extend_by_axiom : string -> ptype -> type_env -> (type_env, error) result