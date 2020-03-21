open Term

type type_env
val new_env : type_env
val type_check : term -> type_env -> ptype option
